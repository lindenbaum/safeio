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
%% Check that `Path` is an accessable path, e.g. a directory to a network
%% mounted file system, that might be blocked due to network problems.
%% @see can_state/2
%% @end
%% ------------------------------------------------------------------------------
-spec can_stat(file:filename()) -> boolean().
can_stat(Path) ->
    can_stat(Path, 5000).

%%------------------------------------------------------------------------------
%% @doc
%% Check that `Path` is an accessable path, e.g. a directory to a network
%% mounted file system, that might be blocked due to network problems.
%%
%% If the `Path` is _safe_ execute the MFA in the calling process.
%%
%% This function will not clog the file I/O of other `file` or `filelib`
%% operations in case the underlying `stat` operation blocks.
%%
%% For example, if an NFS mount is located at "/mnt/nfs" and a file
%% "/mnt/nfs/user-provided/a76f7e62" is expected to be a regular file then
%% `safeio:is_regular("/mnt/nfs", "user-provided/a76f7e62")` will be a safe way
%% to determine if the file is a regular file and even if the NFS mount is
%% stale, this function returns after a short timeout.
%%
%% Also, subsequent calls to `check` will immediately return `false` if a check
%% for `Path`, that was initiated by previous invocation, has not returned yet.
%% This mechanism works by starting an internal server for each distinct `Path`.
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
%% In contrast to {@link filelib:is_regular/1} this function will not
%% clog the file I/O of other `file` or `filelib` operations in case
%% the underlying `stat` operation blocks.
%%
%% For example, if an NFS mount is located at "/mnt/nfs" and a file
%% "/mnt/nfs/user-provided/a76f7e62" is expected to be a regular file
%% then `safeio:is_regular("/mnt/nfs", "user-provided/a76f7e62")`
%% will be a safe way to determine if the file is a regular file and
%% even if the NFS mount is stale, this function returns after a short
%% timeout.
%%
%% Also, subsequent calls to the `is regular` will immediately return
%% `false` if a check for "/mnt/nfs", that was initiated by previous
%% invokation, has not returned yet.
%% This mechanism works by starting an internal server for each distinct
%% `RootDirectory`.
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
