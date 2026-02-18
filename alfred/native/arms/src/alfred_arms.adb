--  Alfred Arms — Les Bras du majordome numerique
--  Programme Ada qui observe et agit sur la machine hote.
--  Protocole : stdin/stdout JSON (une ligne par commande/reponse).
--
--  Commandes :
--    system_info   — hostname, OS, uptime, load average
--    disk_usage    — espace disque par partition
--    memory_usage  — RAM et swap
--    backup        — sauvegarde des donnees Alfred

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings;            use Ada.Strings;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Calendar;           use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Directories;
with GNAT.OS_Lib;

procedure Alfred_Arms is

   --  ================================================================
   --  JSON Helpers (minimal, flat objects only)
   --  ================================================================

   function Escape_JSON (S : String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for I in S'Range loop
         case S (I) is
            when '"'  => Append (Result, "\""");
            when '\'  => Append (Result, "\\");
            when others =>
               if Character'Pos (S (I)) < 32 then
                  Append (Result, " ");
               else
                  Append (Result, S (I));
               end if;
         end case;
      end loop;
      return To_String (Result);
   end Escape_JSON;

   function Extract_Value (Line : String; Key : String) return String is
      Search : constant String := """" & Key & """:";
      Pos    : Natural;
   begin
      Pos := Index (Line, Search);
      if Pos = 0 then
         return "";
      end if;

      --  Skip past the key and colon
      declare
         After_Key : Natural := Pos + Search'Length;
         C         : Character;
      begin
         --  Skip whitespace
         while After_Key <= Line'Last and then
               (Line (After_Key) = ' ' or Line (After_Key) = ASCII.HT) loop
            After_Key := After_Key + 1;
         end loop;

         if After_Key > Line'Last then
            return "";
         end if;

         C := Line (After_Key);

         if C = '"' then
            --  String value: find closing quote
            declare
               Start  : constant Natural := After_Key + 1;
               Finish : Natural := Start;
            begin
               while Finish <= Line'Last and then Line (Finish) /= '"' loop
                  if Line (Finish) = '\' and Finish + 1 <= Line'Last then
                     Finish := Finish + 2;
                  else
                     Finish := Finish + 1;
                  end if;
               end loop;
               return Line (Start .. Finish - 1);
            end;
         else
            --  Numeric or other value: read until comma, brace, or end
            declare
               Start  : constant Natural := After_Key;
               Finish : Natural := Start;
            begin
               while Finish <= Line'Last and then
                     Line (Finish) /= ',' and then
                     Line (Finish) /= '}' and then
                     Line (Finish) /= ' ' loop
                  Finish := Finish + 1;
               end loop;
               return Trim (Line (Start .. Finish - 1), Both);
            end;
         end if;
      end;
   end Extract_Value;

   --  ================================================================
   --  Response writers
   --  ================================================================

   procedure Put_Response (JSON_Body : String) is
   begin
      Put_Line ("{""status"":""ok""," & JSON_Body & "}");
      Flush;
   end Put_Response;

   procedure Put_Error (Msg : String) is
   begin
      Put_Line ("{""status"":""error"",""message"":""" &
                Escape_JSON (Msg) & """}");
      Flush;
   end Put_Error;

   --  ================================================================
   --  File reading helpers
   --  ================================================================

   function Read_First_Line (Path : String) return String is
      F : File_Type;
   begin
      Open (F, In_File, Path);
      declare
         Line : constant String := Get_Line (F);
      begin
         Close (F);
         return Trim (Line, Both);
      end;
   exception
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         return "";
   end Read_First_Line;

   function Read_File_Content (Path : String) return Unbounded_String is
      F      : File_Type;
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      Open (F, In_File, Path);
      while not End_Of_File (F) loop
         Append (Result, Get_Line (F));
         if not End_Of_File (F) then
            Append (Result, ASCII.LF);
         end if;
      end loop;
      Close (F);
      return Result;
   exception
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         return Null_Unbounded_String;
   end Read_File_Content;

   --  ================================================================
   --  system_info
   --  ================================================================

   procedure Cmd_System_Info is
      Hostname : constant String := Read_First_Line ("/etc/hostname");

      --  Parse OS from /etc/os-release
      function Get_OS_Name return String is
         Content : constant Unbounded_String :=
            Read_File_Content ("/etc/os-release");
         S       : constant String := To_String (Content);
         Pos     : Natural;
      begin
         Pos := Index (S, "PRETTY_NAME=""");
         if Pos = 0 then
            return "Linux";
         end if;
         declare
            Start  : constant Natural := Pos + 13;
            Finish : Natural := Start;
         begin
            while Finish <= S'Last and then S (Finish) /= '"' loop
               Finish := Finish + 1;
            end loop;
            return S (Start .. Finish - 1);
         end;
      end Get_OS_Name;

      --  Parse uptime from /proc/uptime
      function Get_Uptime return String is
         Line : constant String := Read_First_Line ("/proc/uptime");
         Pos  : Natural;
         Secs : Natural;
         Days, Hours, Mins : Natural;
      begin
         if Line = "" then
            return "inconnu";
         end if;
         --  Line format: "12345.67 98765.43"
         Pos := Index (Line, ".");
         if Pos = 0 then
            Pos := Index (Line, " ");
            if Pos = 0 then
               Pos := Line'Last + 1;
            end if;
         end if;
         Secs := Natural'Value (Line (Line'First .. Pos - 1));
         Days  := Secs / 86400;
         Hours := (Secs mod 86400) / 3600;
         Mins  := (Secs mod 3600) / 60;
         return Trim (Natural'Image (Days), Left) & "j " &
                Trim (Natural'Image (Hours), Left) & "h " &
                Trim (Natural'Image (Mins), Left) & "m";
      exception
         when others => return "inconnu";
      end Get_Uptime;

      Load_Avg : constant String := Read_First_Line ("/proc/loadavg");
      OS_Name  : constant String := Get_OS_Name;
      Uptime   : constant String := Get_Uptime;

      --  Extract just the 3 load values
      function Get_Load return String is
         Pos : Natural;
      begin
         if Load_Avg = "" then
            return "inconnu";
         end if;
         --  Format: "0.12 0.34 0.56 1/234 5678"
         --  We want just the first 3 numbers
         Pos := Index (Load_Avg, " ", Load_Avg'First);
         if Pos = 0 then
            return Load_Avg;
         end if;
         declare
            Pos2 : Natural := Index (Load_Avg, " ", Pos + 1);
         begin
            if Pos2 = 0 then
               return Load_Avg;
            end if;
            declare
               Pos3 : Natural := Index (Load_Avg, " ", Pos2 + 1);
            begin
               if Pos3 = 0 then
                  Pos3 := Load_Avg'Last + 1;
               end if;
               return Load_Avg (Load_Avg'First .. Pos3 - 1);
            end;
         end;
      end Get_Load;

   begin
      Put_Response (
         """info"":{" &
         """hostname"":""" & Escape_JSON (Hostname) & """," &
         """os"":""" & Escape_JSON (OS_Name) & """," &
         """uptime"":""" & Escape_JSON (Uptime) & """," &
         """load"":""" & Escape_JSON (Get_Load) & """" &
         "}"
      );
   end Cmd_System_Info;

   --  ================================================================
   --  memory_usage
   --  ================================================================

   procedure Cmd_Memory_Usage is
      Content      : constant Unbounded_String :=
         Read_File_Content ("/proc/meminfo");
      S            : constant String := To_String (Content);
      Mem_Total    : Natural := 0;
      Mem_Free     : Natural := 0;
      Mem_Avail    : Natural := 0;
      Swap_Total   : Natural := 0;
      Swap_Free    : Natural := 0;

      function Parse_KB (Key : String) return Natural is
         Pos : Natural;
      begin
         Pos := Index (S, Key & ":");
         if Pos = 0 then
            return 0;
         end if;
         declare
            After : Natural := Pos + Key'Length + 1;
            Start : Natural;
            Finish : Natural;
         begin
            --  Skip whitespace
            while After <= S'Last and then
                  (S (After) = ' ' or S (After) = ASCII.HT) loop
               After := After + 1;
            end loop;
            Start := After;
            --  Read digits
            Finish := Start;
            while Finish <= S'Last and then
                  S (Finish) >= '0' and then S (Finish) <= '9' loop
               Finish := Finish + 1;
            end loop;
            if Finish > Start then
               return Natural'Value (S (Start .. Finish - 1));
            else
               return 0;
            end if;
         end;
      exception
         when others => return 0;
      end Parse_KB;

      Total_MB, Avail_MB, Used_MB : Natural;
      Swap_Total_MB, Swap_Free_MB : Natural;
      Percent_Used : Natural;
      Alert : Boolean;

   begin
      Mem_Total  := Parse_KB ("MemTotal");
      Mem_Free   := Parse_KB ("MemFree");
      Mem_Avail  := Parse_KB ("MemAvailable");
      Swap_Total := Parse_KB ("SwapTotal");
      Swap_Free  := Parse_KB ("SwapFree");

      Total_MB     := Mem_Total / 1024;
      Avail_MB     := Mem_Avail / 1024;
      Used_MB      := Total_MB - Avail_MB;
      Swap_Total_MB := Swap_Total / 1024;
      Swap_Free_MB := Swap_Free / 1024;

      if Total_MB > 0 then
         Percent_Used := ((Total_MB - Avail_MB) * 100) / Total_MB;
      else
         Percent_Used := 0;
      end if;

      Alert := Avail_MB > 0 and then
               Total_MB > 0 and then
               (Avail_MB * 100) / Total_MB < 10;

      Put_Response (
         """memory"":{" &
         """total_mb"":" & Trim (Natural'Image (Total_MB), Left) & "," &
         """used_mb"":" & Trim (Natural'Image (Used_MB), Left) & "," &
         """available_mb"":" & Trim (Natural'Image (Avail_MB), Left) & "," &
         """percent_used"":" & Trim (Natural'Image (Percent_Used), Left) & "," &
         """swap_total_mb"":" & Trim (Natural'Image (Swap_Total_MB), Left) & "," &
         """swap_free_mb"":" & Trim (Natural'Image (Swap_Free_MB), Left) & "," &
         """alert"":" & (if Alert then "true" else "false") &
         "}"
      );
   end Cmd_Memory_Usage;

   --  ================================================================
   --  disk_usage — parse df output
   --  ================================================================

   procedure Cmd_Disk_Usage is
      use GNAT.OS_Lib;

      --  We'll parse /proc/mounts and then use statvfs-like approach
      --  Simpler: spawn df and parse output
      Temp_File  : constant String := "/tmp/alfred_df_output.tmp";
      Df_Path    : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Locate_Exec_On_Path ("df");
      Success    : Boolean;
      Return_Val : Integer;
      Result     : Unbounded_String := To_Unbounded_String ("""partitions"":[");
      First_Part : Boolean := True;
      Has_Alert  : Boolean := False;
      F          : File_Type;
      Line_Num   : Natural := 0;

   begin
      if Df_Path = null then
         Put_Error ("df introuvable");
         return;
      end if;

      --  Run: df -BM --output=target,size,used,avail,pcent
      declare
         Args : GNAT.OS_Lib.Argument_List (1 .. 2);
         Fd   : File_Descriptor;
      begin
         Args (1) := new String'("-BM");
         Args (2) := new String'("--output=target,size,used,avail,pcent");
         Fd := Create_File (Temp_File, Binary);
         Spawn (Df_Path.all, Args, Fd, Return_Val, Err_To_Out => True);
         Close (Fd);
         Free (Df_Path);
         for I in Args'Range loop
            Free (Args (I));
         end loop;
      end;

      if Return_Val /= 0 then
         Put_Error ("df a echoue");
         return;
      end if;

      --  Parse df output
      Open (F, In_File, Temp_File);
      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
         begin
            Line_Num := Line_Num + 1;
            --  Skip header line
            if Line_Num > 1 and then Line'Length > 5 then
               --  Parse: mountpoint  sizeM  usedM  availM  percent%
               declare
                  L      : constant String := Trim (Line, Both);
                  Pos    : Natural := L'First;
                  Mount  : Unbounded_String;
                  Size_S, Used_S, Avail_S, Pct_S : Unbounded_String;
                  Field  : Natural := 0;

                  procedure Skip_Spaces is
                  begin
                     while Pos <= L'Last and then L (Pos) = ' ' loop
                        Pos := Pos + 1;
                     end loop;
                  end Skip_Spaces;

                  procedure Read_Field (Into : out Unbounded_String) is
                     Start : constant Natural := Pos;
                  begin
                     while Pos <= L'Last and then L (Pos) /= ' ' loop
                        Pos := Pos + 1;
                     end loop;
                     Into := To_Unbounded_String (L (Start .. Pos - 1));
                  end Read_Field;

                  Mount_Str : String (1 .. 200);
                  Mount_Len : Natural := 0;
               begin
                  Skip_Spaces;
                  Read_Field (Mount);
                  Skip_Spaces;
                  Read_Field (Size_S);
                  Skip_Spaces;
                  Read_Field (Used_S);
                  Skip_Spaces;
                  Read_Field (Avail_S);
                  Skip_Spaces;
                  Read_Field (Pct_S);

                  --  Filter: only real mount points
                  declare
                     M : constant String := To_String (Mount);
                  begin
                     if M'Length > 0 and then M (M'First) = '/' then
                        --  Remove 'M' suffix from sizes and '%' from percent
                        declare
                           function Strip_Suffix (S : String) return String is
                           begin
                              if S'Length > 1 and then
                                 (S (S'Last) = 'M' or S (S'Last) = '%')
                              then
                                 return S (S'First .. S'Last - 1);
                              else
                                 return S;
                              end if;
                           end Strip_Suffix;

                           Size_Val  : constant String :=
                              Strip_Suffix (To_String (Size_S));
                           Used_Val  : constant String :=
                              Strip_Suffix (To_String (Used_S));
                           Avail_Val : constant String :=
                              Strip_Suffix (To_String (Avail_S));
                           Pct_Val   : constant String :=
                              Strip_Suffix (To_String (Pct_S));
                        begin
                           --  Filter pseudo-filesystems
                           if M = "/" or else M = "/home" or else
                              M = "/tmp" or else M = "/boot" or else
                              M = "/boot/efi" or else
                              (M'Length > 4 and then
                               M (M'First .. M'First + 3) /= "/dev" and then
                               M (M'First .. M'First + 3) /= "/run" and then
                               M (M'First .. M'First + 3) /= "/sys")
                           then
                              if not First_Part then
                                 Append (Result, ",");
                              end if;
                              Append (Result,
                                 "{""mount"":""" & Escape_JSON (M) & """," &
                                 """size_mb"":" & Trim (Size_Val, Both) & "," &
                                 """used_mb"":" & Trim (Used_Val, Both) & "," &
                                 """available_mb"":" & Trim (Avail_Val, Both) & "," &
                                 """percent_used"":" & Trim (Pct_Val, Both) & "}");
                              First_Part := False;
                           end if;

                           --  Check for alert
                           begin
                              declare
                                 Pct : constant Natural :=
                                    Natural'Value (Trim (Pct_Val, Both));
                              begin
                                 if Pct > 90 then
                                    Has_Alert := True;
                                 end if;
                              end;
                           exception
                              when others => null;
                           end;
                        end;
                     end if;
                  end;
               end;
            end if;
         end;
      end loop;
      Close (F);

      --  Clean up temp file
      Ada.Directories.Delete_File (Temp_File);

      Append (Result, "],""alert"":" &
              (if Has_Alert then "true" else "false"));

      Put_Response (To_String (Result));

   exception
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         begin
            Ada.Directories.Delete_File (Temp_File);
         exception
            when others => null;
         end;
         Put_Error ("Erreur lors de l'analyse disque");
   end Cmd_Disk_Usage;

   --  ================================================================
   --  backup
   --  ================================================================

   procedure Cmd_Backup (Data_Dir : String) is
      use GNAT.OS_Lib;
      use Ada.Calendar.Formatting;

      Now        : constant Time := Clock;
      Yr         : Year_Number;
      Mo         : Month_Number;
      Dy         : Day_Number;
      Hr, Mn, Sc : Natural;
      Sub_Sec    : Duration;
      Backup_Dir : constant String := Data_Dir & "/backups";
      Tar_Path   : GNAT.OS_Lib.String_Access :=
         GNAT.OS_Lib.Locate_Exec_On_Path ("tar");
      Return_Val : Integer;
   begin
      if Tar_Path = null then
         Put_Error ("tar introuvable");
         return;
      end if;

      --  Create backup directory
      if not Ada.Directories.Exists (Backup_Dir) then
         Ada.Directories.Create_Directory (Backup_Dir);
      end if;

      --  Build timestamp
      Split (Now, Yr, Mo, Dy, Hr, Mn, Sc, Sub_Sec);

      declare
         function Pad2 (N : Natural) return String is
            S : constant String := Trim (Natural'Image (N), Left);
         begin
            if S'Length = 1 then
               return "0" & S;
            else
               return S;
            end if;
         end Pad2;

         Timestamp  : constant String :=
            Trim (Year_Number'Image (Yr), Left) &
            Pad2 (Mo) & Pad2 (Dy) & "_" &
            Pad2 (Hr) & Pad2 (Mn) & Pad2 (Sc);

         Backup_File : constant String :=
            Backup_Dir & "/alfred_backup_" & Timestamp & ".tar.gz";

         Args : GNAT.OS_Lib.Argument_List (1 .. 5);
         Fd   : File_Descriptor;
      begin
         --  tar czf <backup_file> --exclude=backups -C <parent> <basename>
         declare
            Parent  : constant String := Ada.Directories.Containing_Directory (Data_Dir);
            Base    : constant String := Ada.Directories.Simple_Name (Data_Dir);
         begin
            Args (1) := new String'("czf");
            Args (2) := new String'(Backup_File);
            Args (3) := new String'("--exclude=backups");
            Args (4) := new String'("-C");
            Args (5) := new String'(Parent);

            --  We need to add the base name as an extra arg
            --  Workaround: use a 6-element array
         end;

         --  Free and redo with correct arg count
         for I in Args'Range loop
            Free (Args (I));
         end loop;

         declare
            Args7 : GNAT.OS_Lib.Argument_List (1 .. 7);
            Parent : constant String :=
               Ada.Directories.Containing_Directory (Data_Dir);
            Base   : constant String := Ada.Directories.Simple_Name (Data_Dir);
         begin
            Args7 (1) := new String'("czf");
            Args7 (2) := new String'(Backup_File);
            Args7 (3) := new String'("--exclude=backups");
            Args7 (4) := new String'("--exclude=vaults");
            Args7 (5) := new String'("-C");
            Args7 (6) := new String'(Parent);
            Args7 (7) := new String'(Base);

            --  Spawn tar, discard output
            Fd := Create_File ("/dev/null", Binary);
            Spawn (Tar_Path.all, Args7, Fd, Return_Val, Err_To_Out => True);
            Close (Fd);
            Free (Tar_Path);
            for I in Args7'Range loop
               Free (Args7 (I));
            end loop;
         end;

         if Return_Val /= 0 then
            Put_Error ("tar a echoue (code " &
                       Trim (Integer'Image (Return_Val), Left) & ")");
            return;
         end if;

         --  Get backup file size
         declare
            Size    : constant Ada.Directories.File_Size :=
               Ada.Directories.Size (Backup_File);
            Size_B  : constant Natural := Natural (Size);
            Size_KB : constant Natural := Size_B / 1024;
         begin
            Put_Response (
               """backup"":{" &
               """path"":""" & Escape_JSON (Backup_File) & """," &
               """size_bytes"":" & Trim (Natural'Image (Size_B), Left) & "," &
               """size_kb"":" & Trim (Natural'Image (Size_KB), Left) &
               "}"
            );
         end;
      end;

   exception
      when E : others =>
         Free (Tar_Path);
         Put_Error ("Erreur lors de la sauvegarde");
   end Cmd_Backup;

   --  ================================================================
   --  Main loop
   --  ================================================================

   Line_Buffer : String (1 .. 4096);
   Last        : Natural;

begin
   --  Read commands from stdin, one JSON object per line
   loop
      begin
         Get_Line (Line_Buffer, Last);
      exception
         when End_Error =>
            exit;
      end;

      if Last > 0 then
         declare
            Line : constant String := Line_Buffer (1 .. Last);
            Cmd  : constant String := Extract_Value (Line, "cmd");
         begin
            if Cmd = "system_info" then
               Cmd_System_Info;
            elsif Cmd = "disk_usage" then
               Cmd_Disk_Usage;
            elsif Cmd = "memory_usage" then
               Cmd_Memory_Usage;
            elsif Cmd = "backup" then
               declare
                  Dir : constant String := Extract_Value (Line, "data_dir");
               begin
                  if Dir = "" then
                     Put_Error ("data_dir requis pour backup");
                  else
                     Cmd_Backup (Dir);
                  end if;
               end;
            else
               Put_Error ("Commande inconnue: " & Cmd);
            end if;
         end;
      end if;
   end loop;

end Alfred_Arms;
