safeio
=====

An OTP library for checking that specific directories are safe to use.

This project is the result of problems with handling broken NFS mounts.

Our application would regularily use `filelib:is_regular/1` with a path that
is in an NFS mounted directory.

The problem is, that and whenever the mount point gets stuck, e.g. because of an
NFS-server outage or a network problem, file operations, i.e. calls to `file` and
`filelib` modules block, as soon as the first element is mounted,
even if paths are accessed, that arn't in the broken NFS directory.

This library aims to provide a workaround for this problem by providing a
functions that quickly and safely checks if a path is accessable without going
through `file` or `filelib`.

Build
-----

    $ rebar3 compile

Usage
-----

To check if a file is a regular file, use `safeio:is_regular/2`:

    safeio:is_regular("/mnt/nfs-audio", "en/wav-g711a/test.wav"),
    ...

To execute a function, under the condition that a path is available, use
`with`:

    safeio:with("/mnt/nfs-dir", filelib, is_regular, ["/mnt/nfs-dir/foobar"]),
    ...
