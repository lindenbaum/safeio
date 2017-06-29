-ifndef(sedge____include_safeio_hrl_included).
-define(sedge____include_safeio_hrl_included, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin macros used by other macros
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Macro for all events.
-define(event(Payload), {event, Payload}).

%% Macro for all function return values.
-define(return(Tag, Payload), {return, Tag, Payload}).

%% Macro for all commands.
-define(command(CmdId, Params), {CmdId, Params}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% builtin macros for the user
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% heart-beat command
-define(sedge_heart_beat(HeartbeatPayload), ?command(1, HeartbeatPayload)).

%% heart_beat payload macro
-define(heart_beat(S, R), {heart_beat, S, R}).

%% builtin heart_beat event
-define(event_heart_beat(HeartbeatPayload), ?event(HeartbeatPayload)).

%% leave the read loop
-define(sedge_leave_readloop, ?command(0, {})).

%% configure tracing to a file.
-define(sedge_trace_on(Filename), ?command(2, {Filename})).
-define(sedge_trace_off, ?command(2, {""})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Events
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Records for C-structs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Return Values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(return_can_stat_ok,
  ?return(can_stat, ok)).

-define(return_get_filetype_ok(ARg0),
  ?return(get_filetype, {ok, ARg0})).

-define(return_can_stat_error(ARg0),
  ?return(can_stat, {error, ARg0})).

-define(return_get_filetype_error(ARg0),
  ?return(get_filetype, {error, ARg0})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(can_stat(Path),
  ?command(3, {Path})).

-define(get_filetype(Path),
  ?command(4, {Path})).


-ifdef(sedge_include_funs).

-export([can_stat/2,
         get_filetype/2]).

can_stat(SedgePid, Path) ->
  SedgePid ! ?can_stat(Path).

get_filetype(SedgePid, Path) ->
  SedgePid ! ?get_filetype(Path).

-endif. %% sedge_include_funs.

%%% End of headerfile
-endif.
