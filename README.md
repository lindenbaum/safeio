safeio
=====

An OTP application for checking that specific directories are safe to use.

This project is the result of problems with handling broken NFS mounts.

Our application would regularily use `filelib:is_regular/1` with a path that
is in an NFS mounted directory.

The problem is, whenever such a mount point gets _stuck_, e.g. because of an
NFS-server outage or a network problem, **all** file operations, i.e. calls to
`file` and `filelib` modules, block from the moment the first path in a _stuck_
directory is accessed. Even operations on paths outside the broken mount point
are blocked.

This library aims to provide a workaround for this problem by providing
operations that check if a path is accessable using an external process instead
of `filelib` or `file`.

The library will also try to recognize when an NFS mount is available again.
This is done by periodically killing the external port process (when a stale
mount is detected) and retrying the stat operation.

Build
-----

    $ rebar3 compile

Usage
-----

To check if a file is a regular file, use `safeio:is_regular/2`:

    safeio:is_regular("/mnt/nfs-audio", "en/wav-g711a/test.wav"),
    ...
