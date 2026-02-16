%%%-------------------------------------------------------------------
%%% @doc Alfred Scheduler — Les Muscles d'Alfred.
%%% Un gen_server Erlang pur qui gère les rappels et veille
%%% sur le temps de son maître.
%%% Persistance via Erlang term binary (~/.alfred/data/reminders.dat).
%%%-------------------------------------------------------------------
-module(alfred_scheduler).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_reminder/3, list_reminders/0, check_due/0,
         complete_reminder/1, delete_reminder/1,
         count_pending/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-record(state, {
    reminders = [] :: list(),
    next_id = 1   :: pos_integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Add a reminder. DueAt is a Unix timestamp.
-spec add_reminder(binary(), binary(), integer()) -> {ok, pos_integer()}.
add_reminder(Project, Text, DueAt) ->
    gen_server:call(?MODULE, {add, Project, Text, DueAt}).

%% @doc List all reminders (pending and done).
-spec list_reminders() -> {ok, list()}.
list_reminders() ->
    gen_server:call(?MODULE, list).

%% @doc Return reminders that are due (past their due_at time).
-spec check_due() -> {ok, list()}.
check_due() ->
    gen_server:call(?MODULE, check_due).

%% @doc Mark a reminder as done.
-spec complete_reminder(pos_integer()) -> ok | {error, not_found}.
complete_reminder(Id) ->
    gen_server:call(?MODULE, {complete, Id}).

%% @doc Delete a reminder entirely.
-spec delete_reminder(pos_integer()) -> ok | {error, not_found}.
delete_reminder(Id) ->
    gen_server:call(?MODULE, {delete, Id}).

%% @doc Count pending reminders.
-spec count_pending() -> {ok, non_neg_integer()}.
count_pending() ->
    gen_server:call(?MODULE, count_pending).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Reminders = load_reminders(),
    NextId = compute_next_id(Reminders),
    {ok, #state{reminders = Reminders, next_id = NextId}}.

handle_call({add, Project, Text, DueAt}, _From, State) ->
    #state{reminders = Reminders, next_id = NextId} = State,
    Now = erlang:system_time(second),
    Reminder = #{
        id => NextId,
        project => Project,
        text => Text,
        due_at => DueAt,
        created_at => Now,
        status => pending
    },
    NewReminders = Reminders ++ [Reminder],
    save_reminders(NewReminders),
    {reply, {ok, NextId}, State#state{
        reminders = NewReminders,
        next_id = NextId + 1
    }};

handle_call(list, _From, #state{reminders = Reminders} = State) ->
    {reply, {ok, Reminders}, State};

handle_call(check_due, _From, #state{reminders = Reminders} = State) ->
    Now = erlang:system_time(second),
    Due = [R || R = #{due_at := DueAt, status := Status} <- Reminders,
                Status =:= pending, DueAt =< Now],
    {reply, {ok, Due}, State};

handle_call({complete, Id}, _From, #state{reminders = Reminders} = State) ->
    case find_reminder(Id, Reminders) of
        false ->
            {reply, {error, not_found}, State};
        _ ->
            NewReminders = update_status(Id, done, Reminders),
            save_reminders(NewReminders),
            {reply, ok, State#state{reminders = NewReminders}}
    end;

handle_call({delete, Id}, _From, #state{reminders = Reminders} = State) ->
    case find_reminder(Id, Reminders) of
        false ->
            {reply, {error, not_found}, State};
        _ ->
            NewReminders = [R || R = #{id := RId} <- Reminders, RId =/= Id],
            save_reminders(NewReminders),
            {reply, ok, State#state{reminders = NewReminders}}
    end;

handle_call(count_pending, _From, #state{reminders = Reminders} = State) ->
    Count = length([R || R = #{status := Status} <- Reminders,
                         Status =:= pending]),
    {reply, {ok, Count}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

find_reminder(_Id, []) -> false;
find_reminder(Id, [R = #{id := Id} | _]) -> R;
find_reminder(Id, [_ | Rest]) -> find_reminder(Id, Rest).

update_status(Id, NewStatus, Reminders) ->
    lists:map(
        fun(R = #{id := RId}) when RId =:= Id ->
                R#{status := NewStatus};
           (R) ->
                R
        end,
        Reminders
    ).

compute_next_id([]) -> 1;
compute_next_id(Reminders) ->
    MaxId = lists:max([Id || #{id := Id} <- Reminders]),
    MaxId + 1.

%% Persistence — Erlang native term storage

data_path() ->
    Home = os:getenv("HOME"),
    filename:join([Home, ".alfred", "data", "reminders.dat"]).

load_reminders() ->
    Path = data_path(),
    case file:read_file(Path) of
        {ok, Data} ->
            try binary_to_term(Data)
            catch _:_ -> []
            end;
        {error, enoent} ->
            []
    end.

save_reminders(Reminders) ->
    Path = data_path(),
    Data = term_to_binary(Reminders),
    file:write_file(Path, Data).
