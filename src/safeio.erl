-module(safeio).

%% API exports
-export([can_stat/1,
         can_stat/2,
         is_regular/2,
         is_regular/3,
         info/0,
         info/1
        ]).

-export_type([ioerror/0, filetype/0]).

-type ioerror() ::
          permission_denied
        | io_error
        | not_found
        | bad_path
        | other_error.

-type filetype() ::
          regular_file
        | directory
        | other_filetype.

%%====================================================================
%% API functions
%%====================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Return true if calling `stat` on `Path` was successful.
%% @see can_stat/2
%% @end
%% ------------------------------------------------------------------------------
-spec can_stat(file:filename()) -> boolean().
can_stat(Path) ->
    can_stat(Path, 5000).

%%------------------------------------------------------------------------------
%% @doc
%% Return true if calling `stat` on `Path` was successful.
%%
%% This function will not cause `file` or `filelib` operations to block, in case
%% the underlying `stat` operation blocks. This is in contrast to `file` and
%% `filelib` behaviour, where `filelib:is_regular/1` on a path that is in a
%% stale NFS mount will cause other `filelib:...` operations on totally
%% unrelated paths to block, too.
%%
%% Also, subsequent invocations will immediately return `false` if a check
%% for `Path`, that was initiated by previous invocation, has not returned yet.
%%
%% This mechanism works by starting an internal server for each distinct `Path`,
%% see {@link info/1}.
%%
%% @end
%% ------------------------------------------------------------------------------
-spec can_stat(file:filename(), timeout()) -> boolean().
can_stat(Path, TimeOutMillis) ->
    case safeio_sup:get_directory_guard(Path) of
        {ok, Port} ->
            case safeio_directory_guard:can_stat(Port, TimeOutMillis) of
                ok ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Check that `RootDirectory++"/"++RelativePath` is a regular file.
%% Return `false` if filesystem errors or timeouts were encountered.
%% @see is_regular/3
%% @end
%%------------------------------------------------------------------------------
-spec is_regular(file:filename(), file:filename()) -> boolean().
is_regular(RootDirectory, RelativePath) ->
    is_regular(RootDirectory, RelativePath, 5000).

%%------------------------------------------------------------------------------
%% @doc
%% Check that `RootDirectory++"/"++RelPath` is a re file.
%% Return `false` if filesystem errors or timeouts were encountered.
%%
%% The non-functional behaviour regarding safe access to directories is the same
%% as in {@link can_stat/2}.
%%
%% @end
%%------------------------------------------------------------------------------
-spec is_regular(file:filename(), file:filename(), timeout()) -> boolean().
is_regular(RootDir, RelPath, TimeOut) ->
    case safeio_sup:get_directory_guard(RootDir) of
        {ok, Port} ->
            case safeio_directory_guard:get_filetype(Port, RelPath, TimeOut) of
                regular_file ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.


%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec info() -> ok.
info() ->
    [safeio_directory_guard:info(G) || G <- safeio_sup:which_directory_guards()].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec info(file:filename()) -> ok.
info(RootDir) ->
    case safeio_sup:get_directory_guard(RootDir) of
        {ok, Port} ->
            safeio_directory_guard:info(Port);
        _ ->
            ok
    end.


%%====================================================================
%% Internal functions
%%====================================================================
