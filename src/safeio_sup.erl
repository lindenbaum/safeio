-module(safeio_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
        get_directory_guard/1,
        which_directory_guards/0]).

%% Supervisor callbacks
-export([init/1]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec get_directory_guard(string()) -> {ok, pid()} | {error, term()}.
get_directory_guard(Path) ->
    case supervisor:start_child(?MODULE, childspec(Path)) of
        {ok, Child} ->
            {ok, Child};
        {ok, Child, _} ->
            {ok, Child};
        {error, {already_started, Child}} ->
            {ok, Child};
        {error, already_represent} ->
            case supervisor:restart_child(?MODULE, Path) of
                {ok, Child} ->
                    {ok, Child};
                {ok, Child, _} ->
                    {ok, Child};
                {error, not_found} ->
                    ?MODULE:get_directory_guard(Path);
                {error, running} ->
                    ?MODULE:get_directory_guard(Path)
            end;
        {error, StartError} ->
            {error, StartError}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec which_directory_guards() -> [pid()].
which_directory_guards() ->
    Cs = supervisor:which_children(?MODULE),
    [G || {_, G, _, [safeio_directory_guard]} <- Cs, is_pid(G)].

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%%------------------------------------------------------------------------------
init([]) -> {ok, {{one_for_one, 1000, 1}, []}}.

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
childspec(Path) ->
    {Path,
     {safeio_directory_guard, start_link, [Path]},
     transient,
     1000,
     worker,
     [safeio_directory_guard]}.
