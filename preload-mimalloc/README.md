# Vendored mimalloc


This is [mimalloc](https://github.com/microsoft/mimalloc) vendored for use in
our server code. You can upgrade mimalloc by editing `VERSION` in
`update-sources.sh` and re-running it.

Usage sites in haskell projects look like:

    -- See 'preload-mimalloc/README.md'
    -- We want this in all server binary executable sections
    common preload-mimalloc
        c-sources:       ../preload-mimalloc/mimalloc/src/static.c
        include-dirs:    ../preload-mimalloc/mimalloc/include
        cc-options:      -DMI_MALLOC_OVERRIDE

## Usage in haskell projects

In [#9447](https://github.com/hasura/graphql-engine-mono/pull/9447) we see
overloading the server with large and slow queries results in high memory usage
and what looks like a space leak, involving foreign data from libpq.

See that ticket for details, but mimalloc helps (relative to my glibc) by:

- better avoiding fragmentation, resulting in lower overall memory usage (4GB peak 2.8 aftervs.
- eagerly decomitting memory so that OS-reported memory usage is a useful
  metric (we do the same in the haskell RTS by specifying
  `--disable-delayed-os-memory-return`)

We also benefit by standardizing this, rather than relying on users' glibc
version, which might vary in behavior, making debugging difficult.

Other implementations such as tcmalloc or jemalloc were not tested but might
perform better or be more tunable.

### Why not LD_PRELOAD?

It's most common and simplest to use the dynamic linker to override malloc.
This would work in docker containers we ship, but we want all users to have the
same (better) behavior out of the box.
