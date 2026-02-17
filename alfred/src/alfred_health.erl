%%%-------------------------------------------------------------------
%%% @doc Alfred Health Monitor — Diagnostic des organes d'Alfred.
%%% Module Erlang pur qui vérifie l'état de chaque composant.
%%% Mis à jour pour le système multi-vault (creator, users, culture).
%%%-------------------------------------------------------------------
-module(alfred_health).

-export([check_all/0, check_beam/0, check_vault/0, check_storage/0,
         check_scheduler/0, check_brain/0, check_cortex/0, check_mistral/0]).

%% @doc Check the health of all Alfred components.
-spec check_all() -> list({atom(), map()}).
check_all() ->
    [
        {beam, check_beam()},
        {vault, check_vault()},
        {storage, check_storage()},
        {scheduler, check_scheduler()},
        {brain, check_brain()},
        {cortex, check_cortex()},
        {mistral, check_mistral()}
    ].

%% @doc BEAM VM status.
check_beam() ->
    {WallClock, _} = erlang:statistics(wall_clock),
    #{
        status => ok,
        otp_release => list_to_binary(erlang:system_info(otp_release)),
        erts_version => list_to_binary(erlang:system_info(version)),
        process_count => erlang:system_info(process_count),
        memory_mb => erlang:memory(total) div (1024 * 1024),
        uptime_ms => WallClock
    }.

%% @doc Check if the Zig vault binary and vault files are available.
check_vault() ->
    Paths = [
        "native/vault/zig-out/bin/alfred-vault"
    ],
    Found = lists:any(fun(P) -> filelib:is_file(P) end, Paths),
    case Found of
        true ->
            VaultDir = vault_dir_path(),
            CreatorExists = filelib:is_file(filename:join(VaultDir, "creator.enc")),
            UsersExists = filelib:is_file(filename:join(VaultDir, "users.enc")),
            CultureExists = filelib:is_file(filename:join(VaultDir, "culture.enc")),
            AllExist = CreatorExists andalso UsersExists andalso CultureExists,
            VaultCount = length([X || X <- [CreatorExists, UsersExists, CultureExists], X =:= true]),
            %% Check for legacy vault too
            LegacyExists = filelib:is_file(legacy_vault_path()),
            #{
                status => ok,
                binary_found => true,
                vault_exists => AllExist,
                vault_count => VaultCount,
                creator_exists => CreatorExists,
                users_exists => UsersExists,
                culture_exists => CultureExists,
                legacy_exists => LegacyExists
            };
        false ->
            #{
                status => warning,
                binary_found => false,
                vault_exists => false,
                vault_count => 0,
                creator_exists => false,
                users_exists => false,
                culture_exists => false,
                legacy_exists => false
            }
    end.

%% @doc Check if the data storage directory is writable.
check_storage() ->
    DataDir = data_dir(),
    case filelib:is_dir(DataDir) of
        true ->
            TestFile = filename:join(DataDir, ".health_check"),
            case file:write_file(TestFile, <<"ok">>) of
                ok ->
                    file:delete(TestFile),
                    #{
                        status => ok,
                        data_dir => list_to_binary(DataDir),
                        writable => true
                    };
                {error, _} ->
                    #{
                        status => error,
                        data_dir => list_to_binary(DataDir),
                        writable => false
                    }
            end;
        false ->
            #{
                status => error,
                data_dir => list_to_binary(DataDir),
                writable => false
            }
    end.

%% @doc Check if the scheduler gen_server is running.
check_scheduler() ->
    case whereis(alfred_scheduler) of
        undefined ->
            #{status => down, running => false, reminders => 0};
        Pid when is_pid(Pid) ->
            {ok, Count} = alfred_scheduler:count_pending(),
            #{status => ok, running => true, reminders => Count}
    end.

%% @doc Check if the Julia brain is available.
check_brain() ->
    JuliaPaths = [
        filename:join([os:getenv("HOME"), ".juliaup", "bin", "julia"]),
        "/usr/local/bin/julia",
        "/usr/bin/julia"
    ],
    JuliaFound = lists:any(fun(P) -> filelib:is_file(P) end, JuliaPaths),
    ScriptPath = "native/brain/src/main.jl",
    ScriptFound = filelib:is_file(ScriptPath),
    case {JuliaFound, ScriptFound} of
        {true, true} ->
            #{status => ok, julia_found => true, script_found => true};
        {true, false} ->
            #{status => warning, julia_found => true, script_found => false};
        {false, _} ->
            #{status => warning, julia_found => false, script_found => ScriptFound}
    end.

%% @doc Check if R/Rscript is available.
check_cortex() ->
    RscriptPaths = ["/usr/bin/Rscript", "/usr/local/bin/Rscript"],
    RFound = lists:any(fun(P) -> filelib:is_file(P) end, RscriptPaths),
    ScriptPath = "native/cortex/src/main.R",
    ScriptFound = filelib:is_file(ScriptPath),
    case {RFound, ScriptFound} of
        {true, true} ->
            #{status => ok, r_found => true, script_found => true};
        {true, false} ->
            #{status => warning, r_found => true, script_found => false};
        {false, _} ->
            #{status => warning, r_found => false, script_found => ScriptFound}
    end.

%% @doc Check if Mistral API key is configured.
check_mistral() ->
    case os:getenv("MISTRAL_API_KEY") of
        false ->
            %% Check if vault directory exists (key might be in creator vault)
            VaultDir = vault_dir_path(),
            case filelib:is_file(filename:join(VaultDir, "creator.enc")) of
                true ->
                    #{status => ok, source => vault, configured => possible};
                false ->
                    #{status => warning, source => none, configured => false}
            end;
        _Key ->
            #{status => ok, source => env, configured => true}
    end.

%%====================================================================
%% Internal
%%====================================================================

data_dir() ->
    Home = os:getenv("HOME"),
    filename:join([Home, ".alfred", "data"]).

vault_dir_path() ->
    Home = os:getenv("HOME"),
    filename:join([Home, ".alfred", "vaults"]).

legacy_vault_path() ->
    Home = os:getenv("HOME"),
    filename:join([Home, ".alfred", "vault.enc"]).
