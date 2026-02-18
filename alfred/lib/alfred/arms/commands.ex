defmodule Alfred.Arms.Commands do
  @moduledoc """
  Commandes des Bras (Ada) — observation et action sur la machine hôte.
  """

  alias Alfred.Butler
  alias Alfred.Arms.Port

  def handle(["status"]) do
    case Port.send_command(%{cmd: "system_info"}) do
      {:ok, %{"info" => info}} ->
        Butler.say("Monsieur, voici l'état de votre machine :\n")
        IO.puts("  Machine   : #{info["hostname"]}")
        IO.puts("  Système   : #{info["os"]}")
        IO.puts("  Uptime    : #{info["uptime"]}")
        IO.puts("  Charge    : #{info["load"]}")
        IO.puts("")

      {:error, reason} ->
        Butler.say("Je suis navré Monsieur, impossible d'observer la machine : #{reason}")
    end
  end

  def handle(["disk"]) do
    case Port.send_command(%{cmd: "disk_usage"}) do
      {:ok, %{"partitions" => partitions, "alert" => alert}} ->
        Butler.say("Monsieur, voici l'utilisation de vos disques :\n")

        Enum.each(partitions, fn p ->
          pct = p["percent_used"]
          icon = if pct >= 90, do: "!", else: "○"
          size_gb = Float.round(p["size_mb"] / 1024, 1)
          avail_gb = Float.round(p["available_mb"] / 1024, 1)

          IO.puts(
            "  #{icon} #{p["mount"]}  — #{avail_gb} Go libres / #{size_gb} Go (#{pct}% utilisé)"
          )
        end)

        if alert do
          IO.puts("")
          IO.puts("  /!\\ Attention : espace disque critique sur une ou plusieurs partitions !")
        end

        IO.puts("")

      {:error, reason} ->
        Butler.say("Je suis navré Monsieur, impossible d'analyser les disques : #{reason}")
    end
  end

  def handle(["memory"]) do
    case Port.send_command(%{cmd: "memory_usage"}) do
      {:ok, %{"memory" => mem}} ->
        Butler.say("Monsieur, voici l'état de la mémoire :\n")

        total_gb = Float.round(mem["total_mb"] / 1024, 1)
        used_gb = Float.round(mem["used_mb"] / 1024, 1)
        avail_gb = Float.round(mem["available_mb"] / 1024, 1)

        IO.puts("  RAM       : #{used_gb} Go utilisés / #{total_gb} Go (#{mem["percent_used"]}%)")
        IO.puts("  Disponible: #{avail_gb} Go")

        if mem["swap_total_mb"] > 0 do
          swap_total_gb = Float.round(mem["swap_total_mb"] / 1024, 1)
          swap_free_gb = Float.round(mem["swap_free_mb"] / 1024, 1)
          IO.puts("  Swap      : #{swap_free_gb} Go libres / #{swap_total_gb} Go")
        end

        if mem["alert"] do
          IO.puts("")
          IO.puts("  /!\\ Attention : mémoire disponible critique !")
        end

        IO.puts("")

      {:error, reason} ->
        Butler.say("Je suis navré Monsieur, impossible d'analyser la mémoire : #{reason}")
    end
  end

  def handle(["backup"]) do
    data_dir = Path.expand("~/.alfred")

    Butler.say("Sauvegarde en cours, Monsieur...")

    case Port.send_command(%{cmd: "backup", data_dir: data_dir}) do
      {:ok, %{"backup" => backup}} ->
        size = format_size(backup["size_bytes"] || backup["size_kb"] * 1024)
        Butler.say("Sauvegarde terminée avec succès.")
        IO.puts("  Fichier : #{backup["path"]}")
        IO.puts("  Taille  : #{size}")
        IO.puts("")

      {:error, reason} ->
        Butler.say("Je suis navré Monsieur, erreur lors de la sauvegarde : #{reason}")
    end
  end

  def handle([]) do
    handle(["help"])
  end

  def handle(["help"]) do
    Butler.say("Monsieur, les commandes des Bras (Ada) sont :\n")

    IO.puts("""
      alfred arms status    État de la machine (hostname, OS, uptime, charge)
      alfred arms disk      Utilisation des disques
      alfred arms memory    État de la mémoire (RAM, swap)
      alfred arms backup    Sauvegarder les données d'Alfred
    """)
  end

  def handle(unknown) do
    Butler.say("Monsieur, commande inconnue : arms #{Enum.join(unknown, " ")}")
    handle(["help"])
  end

  # -- Helpers --

  defp format_size(bytes) when is_integer(bytes) do
    cond do
      bytes >= 1_048_576 ->
        mb = Float.round(bytes / 1_048_576, 1)
        "#{mb} Mo"

      bytes >= 1024 ->
        kb = Float.round(bytes / 1024, 1)
        "#{kb} Ko"

      true ->
        "#{bytes} octets"
    end
  end

  defp format_size(_), do: "inconnue"
end
