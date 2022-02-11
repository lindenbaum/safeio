%%%=============================================================================
%%% @doc
%%% A process that guards file access for a certain root directory against
%%% being (e.g. NFS-) blocked.
%%% @end
%%%=============================================================================
-module(safeio_directory_guard).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    can_stat/1,
    can_stat/2,
    get_filetype/2,
    get_filetype/3,
    info/1,
    trace_on/2,
    trace_off/1,
    heart_beat/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("safeio.hrl").

-define(CHECK_TIMEOUT_MILLIS, 1000).
-define(CHECK_ALIVE_INTERVAL, 30000).

-type from() :: {pid(), term()}.

-type operation() :: can_stat | {get_filetype, file:filename()}.

-record(state, {
    port :: port() | no_port,
    path :: string(),
    pending :: [{operation(), from()}],
    req_start_time :: non_neg_integer() | no_req_start_time,
    success_count :: integer(),
    fail_count :: integer(),
    timer :: reference()
}).

-define(NO_PENDING_REQUESTS(S), S = #state{pending = []}).

%%%=============================================================================
%%% Public API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Run the safeio native binary and configure it.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(string()) -> {ok, pid()} | {error, term()}.
start_link(Path) -> gen_server:start_link(?MODULE, [Path], []).

%%------------------------------------------------------------------------------
%% @doc
%% Print information about the port process status.
%% @end
%%------------------------------------------------------------------------------
-spec info(pid()) -> ok.
info(Pid) when is_pid(Pid) ->
    #state{
        port = Port,
        path = Path,
        pending = Ps,
        req_start_time = TStart,
        success_count = SC,
        fail_count = FC
    } =
        gen_server:call(Pid, info),
    io:format("Directory Guard Statistics:~n"),
    io:format(" * Path:          ~s~n", [Path]),
    io:format(" * Successful:    ~w~n", [SC]),
    io:format(" * Failed:        ~w~n", [FC]),
    io:format(" * Pending:       ~w~n", [length(Ps)]),
    case TStart of
        no_req_start_time ->
            ok;
        _ ->
            T = now_millis() - TStart,
            io:format(" * Pending Since: ~w~n", [T])
    end,
    io:format(" * Port:          ~s~n", [erlang:port_to_list(Port)]),
    io:format(" * OS_PID:        ~w~n", [extract_port_info(Port, os_pid)]),
    io:format(" * I/O:           ~w/~wB~n", [
        extract_port_info(Port, input),
        extract_port_info(Port, output)
    ]),
    io:format(" * Memory:        ~wB~n", [extract_port_info(Port, memory)]).

%%------------------------------------------------------------------------------
%% @doc
%% Check if the directory is accessable.
%% @end
%%------------------------------------------------------------------------------
-spec can_stat(pid()) -> ok | {error, term()}.
can_stat(Pid) -> can_stat(Pid, ?CHECK_TIMEOUT_MILLIS).

%%------------------------------------------------------------------------------
%% @doc
%% Check if the directory is accessable, with explicit time out.
%% @end
%%------------------------------------------------------------------------------
-spec can_stat(pid(), timeout()) -> ok | {error, term()}.
can_stat(Pid, TimeOutMillis) when is_pid(Pid) ->
    try
        gen_server:call(Pid, {can_stat, TimeOutMillis}, TimeOutMillis)
    catch
        exit:_ -> {error, timeout}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Get the filetype of a path relative to the directory passed in to
%% `start_link'.
%% @end
%%------------------------------------------------------------------------------
-spec get_filetype(pid(), file:filename()) ->
    safeio:filetype() | {error, term()}.
get_filetype(Pid, RelPathIn) ->
    get_filetype(Pid, RelPathIn, ?CHECK_TIMEOUT_MILLIS).

%%------------------------------------------------------------------------------
%% @doc
%% Check if the directory is accessable.
%% @end
%%------------------------------------------------------------------------------
-spec get_filetype(pid(), file:filename(), timeout()) ->
    safeio:filetype() | {error, term()}.
get_filetype(Pid, RelPathIn, TimeOutMillis) when is_pid(Pid) ->
    case safe_relative_path(RelPathIn) of
        unsafe ->
            {error, {not_a_relative_path, RelPathIn}};
        RelPath ->
            try
                gen_server:call(
                    Pid,
                    {{get_filetype, RelPath}, TimeOutMillis},
                    TimeOutMillis
                )
            catch
                exit:_ -> {error, timeout}
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Enable sedge traces to a file.
%% @end
%%------------------------------------------------------------------------------
-spec trace_on(pid(), file:filename_all()) -> ok.
trace_on(Pid, LogFile) -> cast_to_port(Pid, ?sedge_trace_on(LogFile)).

%%------------------------------------------------------------------------------
%% @doc
%% Disable sedge traces.
%% @end
%%------------------------------------------------------------------------------
-spec trace_off(pid()) -> ok.
trace_off(P) -> cast_to_port(P, ?sedge_trace_off).

%%------------------------------------------------------------------------------
%% @doc
%% Make an empty roundtrip to the port process and log the number of seconds
%% that it took.
%% @end
%%------------------------------------------------------------------------------
-spec heart_beat(pid()) -> ok.
heart_beat(P) ->
    cast_to_port(P, ?sedge_heart_beat({?heart_beat(0, now_millis())})).

%%%=============================================================================
%%% API Helper functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec cast_to_port(pid(), term()) -> ok.
cast_to_port(P, Term) -> gen_server:cast(P, {cast_to_port, Term}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([Path]) ->
    process_flag(trap_exit, true),
    {ok, #state{
        port = start_port(),
        path = Path,
        pending = [],
        req_start_time = no_req_start_time,
        success_count = 0,
        fail_count = 0,
        timer = start_check()
    }}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(info, _From, State) ->
    {reply, State, State};
handle_call({Op, TimeOutMillis}, From, State) ->
    case is_blocked_for_too_long(State, TimeOutMillis) of
        true -> {reply, {error, timeout}, add_fail_count(1, State)};
        false -> {noreply, enque_and_execute(Op, From, State)}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({cast_to_port, Cmd}, State) ->
    {noreply, cast_to_port_impl(State, Cmd)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({Port, {data, Data}}, State = #state{port = Port}) ->
    handle_info(binary_to_term(Data), State);
handle_info({Port, {exit_status, Status}}, State = #state{port = Port}) ->
    {stop, {port_exited, Status}, State#state{port = no_port}};
handle_info(?event(Payload), State) ->
    handle_port_event(Payload),
    {noreply, State};
handle_info(Return = ?return(_, _), State = #state{}) ->
    {noreply,
        if_idle_execute_next_pending(
            handle_port_return(
                Return,
                reset_req_start_time(State)
            )
        )};
handle_info(?sedge_leave_readloop, State) ->
    {stop, normal, State};
handle_info({timeout, Ref, check}, State = #state{timer = Ref}) ->
    %% The idea behind this:
    %% Create a recurring timer, that checks whether the last operation is still
    %% in progress (which means it is blocked). In this case crash this server,
    %% which kills the corresponding port process and let it get restarted in
    %% the supervisor.
    case is_blocked_for_too_long(State, ?CHECK_ALIVE_INTERVAL) of
        true -> {stop, io_blocked, State};
        false -> {noreply, State#state{timer = start_check()}}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(Reason, State) -> maybe_shutdown(fail_all(Reason, State)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec is_blocked_for_too_long(#state{}, timeout()) -> boolean().
is_blocked_for_too_long(_, infinity) ->
    false;
is_blocked_for_too_long(#state{req_start_time = no_req_start_time}, _) ->
    false;
is_blocked_for_too_long(#state{req_start_time = T0}, TimeOutMillis) ->
    Now = now_millis(),
    (Now - T0) > TimeOutMillis.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec enque_and_execute(operation(), from(), #state{}) -> #state{}.
enque_and_execute(Op, From, State) ->
    if_idle_execute_next_pending(enque_pending(Op, From, State)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec enque_pending(operation(), from(), #state{}) -> #state{}.
enque_pending(Op, From, State = #state{pending = Ps}) ->
    State#state{pending = Ps ++ [{Op, From}]}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec clear_pending(#state{}) -> #state{}.
clear_pending(State) -> State#state{pending = []}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec op_to_cmd(operation(), #state{}) -> ?command(term(), term()).
op_to_cmd({get_filetype, RelPath}, State) ->
    ?get_filetype(filename:join(State#state.path, RelPath));
op_to_cmd(can_stat, State) ->
    ?can_stat(State#state.path).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec set_req_start_time(#state{}) -> #state{}.
set_req_start_time(State) -> State#state{req_start_time = now_millis()}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec if_idle_execute_next_pending(#state{}) -> #state{}.
if_idle_execute_next_pending(State) ->
    case {State#state.pending, State#state.req_start_time} of
        {[{Op, _} | _], no_req_start_time} ->
            set_req_start_time(
                cast_to_port_impl(
                    State,
                    op_to_cmd(Op, State)
                )
            );
        _ ->
            State
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec handle_port_return(term(), #state{}) -> #state{}.
handle_port_return(?return_get_filetype_ok(T), State) ->
    succeed_pending_get_filetype(filetype_to_atom(T), State);
handle_port_return(?return_get_filetype_error(E), State) ->
    fail_pending_get_filetype(ioerror_to_atom(E), State);
handle_port_return(?return_can_stat_ok, State) ->
    succeed_pending_can_stat(State);
handle_port_return(?return_can_stat_error(E), State) ->
    fail_all(ioerror_to_atom(E), State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec succeed_pending_can_stat(#state{}) -> #state{}.
succeed_pending_can_stat(State = #state{pending = Ps}) ->
    {PsNew, Successes} =
        lists:foldr(
            fun({Op, From}, {NewPs, Successes}) ->
                case Op of
                    can_stat ->
                        gen_server:reply(From, ok),
                        {NewPs, Successes + 1};
                    _ ->
                        {[{Op, From} | NewPs], Successes}
                end
            end,
            {[], 0},
            Ps
        ),
    add_success_count(
        Successes,
        State#state{pending = PsNew}
    ).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec fail_all(term(), #state{}) -> #state{}.
fail_all(E, State = #state{pending = Ps}) ->
    lists:map(
        fun({_, P}) ->
            gen_server:reply(P, {error, E})
        end,
        Ps
    ),
    add_fail_count(length(Ps), clear_pending(State)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec succeed_pending_get_filetype(safeio:filetype(), #state{}) -> #state{}.
succeed_pending_get_filetype(FileType, State = #state{pending = Ps}) ->
    {PsNew, Successes} =
        case Ps of
            [{{get_filetype, _}, From} | Rest] ->
                gen_server:reply(From, FileType),
                {Rest, 1};
            _ ->
                {Ps, 0}
        end,
    add_success_count(
        Successes,
        State#state{pending = PsNew}
    ).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec fail_pending_get_filetype(safeio:ioerror(), #state{}) -> #state{}.
fail_pending_get_filetype(E, State = #state{pending = Ps}) ->
    {PsRest, FailCount} =
        case Ps of
            [{{get_filetype, _}, From} | Rest] ->
                gen_server:reply(From, {error, E}),
                {Rest, 1};
            _ ->
                {Ps, 0}
        end,
    add_fail_count(
        FailCount,
        State#state{pending = PsRest}
    ).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec reset_req_start_time(#state{}) -> #state{}.
reset_req_start_time(State) ->
    State#state{req_start_time = no_req_start_time}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
add_success_count(N, State = #state{success_count = C}) ->
    State#state{success_count = C + N}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
add_fail_count(N, State = #state{fail_count = C}) ->
    State#state{fail_count = C + N}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_port_event(?heart_beat(Sent, Received)) ->
    io:format(
        "heartbeat event - "
        "sent: ~wms, "
        "received by port: ~ws, ",
        [Sent, Received]
    ).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
cast_to_port_impl(State = #state{port = Port}, Term) ->
    erlang:port_command(Port, term_to_binary(Term)),
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_shutdown(#state{port = no_port}) ->
    ok;
maybe_shutdown(#state{port = Port}) ->
    try erlang:port_info(Port, os_pid) of
        {os_pid, Process} ->
            Cmd = io_lib:format("kill -s KILL ~w", [Process]),
            os:cmd(lists:flatten(Cmd)),
            shutdown_loop(Port);
        _ ->
            ok
    catch
        _:_ -> ok
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
shutdown_loop(Port) ->
    receive
        {Port, {exit_status, _}} ->
            ok;
        {Port, closed} ->
            shutdown_loop(Port);
        {'EXIT', _Port, _Reason} ->
            shutdown_loop(Port);
        {Port, {data, _Data}} ->
            shutdown_loop(Port)
    after 10000 ->
        ok
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
extract_port_info(Port, Item) ->
    case erlang:port_info(Port, Item) of
        {Item, Value} -> Value;
        undefined -> undefined
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
now_millis() -> erlang:system_time() div 1000000.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start_check() -> erlang:start_timer(?CHECK_ALIVE_INTERVAL, self(), check).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start_port() ->
    PortOpts = [{packet, 2}, binary, exit_status, nouse_stdio],
    open_port({spawn, get_cmd()}, PortOpts).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_cmd() -> string:join([valgrind(), exe()], " ").

%%------------------------------------------------------------------------------
%% @private
%% If the environment var SAFEIO_PORT_USE_VALGRIND is set to "1", activate
%% valgrind like this (memcheck):
%%------------------------------------------------------------------------------
valgrind() ->
    case os:getenv("SAFEIO_PORT_USE_VALGRIND") of
        V when V == ""; V == "1" ->
            "valgrind --log-file=/tmp/safeio_port.valgrind --leak-check=full"
            " --show-leak-kinds=all";
        false ->
            ""
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
exe() -> filename:join([code:priv_dir(safeio), "safeio_port"]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec ioerror_to_atom(0..4) -> safeio:ioerror().
ioerror_to_atom(0) -> permission_denied;
ioerror_to_atom(1) -> io_error;
ioerror_to_atom(2) -> not_found;
ioerror_to_atom(3) -> bad_path;
ioerror_to_atom(4) -> other_error.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec filetype_to_atom(0..2) -> safeio:filetype().
filetype_to_atom(0) -> regular_file;
filetype_to_atom(1) -> directory;
filetype_to_atom(2) -> other_filetype.

%%------------------------------------------------------------------------------
%% @private
%% This function is borrowed from the OTP filename module.
%%------------------------------------------------------------------------------
-spec safe_relative_path(file:filename()) -> unsafe | file:filename().
safe_relative_path(Path) ->
    case filename:pathtype(Path) of
        relative ->
            Cs0 = filename:split(Path),
            safe_relative_path_1(Cs0, []);
        _ ->
            unsafe
    end.

%%------------------------------------------------------------------------------
%% @private
%% This function is borrowed from the OTP filename module.
%%------------------------------------------------------------------------------
safe_relative_path_1(["." | T], Acc) -> safe_relative_path_1(T, Acc);
safe_relative_path_1([<<".">> | T], Acc) -> safe_relative_path_1(T, Acc);
safe_relative_path_1([".." | T], Acc) -> climb(T, Acc);
safe_relative_path_1([<<"..">> | T], Acc) -> climb(T, Acc);
safe_relative_path_1([H | T], Acc) -> safe_relative_path_1(T, [H | Acc]);
safe_relative_path_1([], []) -> [];
safe_relative_path_1([], Acc) -> filename:join(lists:reverse(Acc)).

%%------------------------------------------------------------------------------
%% @private
%% This function is borrowed from the OTP filename module.
%%------------------------------------------------------------------------------
climb(_, []) -> unsafe;
climb(T, [_ | Acc]) -> safe_relative_path_1(T, Acc).

%%%% {ok, C2} = safeio_directory_guard:start_link("/mnt/audio"), dbg:tracer(), dbg:p(C2, [p,m]), safeio_directory_guard:heart_beat(C2).
%%%% [spawn(fun() -> safeio_directory_guard:can_stat(C2) end) || _ <- lists:seq(1,100)], safeio_directory_guard:info(C2).
%%%% {ok, C3} = safeio_directory_guard:start_link("/mnt/audio"), dbg:tracer(), dbg:p(C3, [p,m]), safeio_directory_guard:can_stat(C3).
%%%% {ok, C4} = safeio_directory_guard:start_link("/mnt"), dbg:tracer(), dbg:p(C4, [p,m]), safeio_directory_guard:can_stat(C4).
