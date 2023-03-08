At the top level we have a `cabal.project` file that defines project
configuration settings that stay the same, regardless of whether we're doing
local development, building on CI, etc.

Additionally in this directory we have various `cabal.project.local` files that
add or override settings depending on context:

- `ci-*.project.local` - these are used when building in CI (see `.buildkite/`)
- `dev-sh.project.local` - this is used to configure the environment we expect in 
  `scripts/dev.sh`, and is where we put good local development defaults
- `dev-sh-optimized.project.local` - above, but building with optimizations,
   suitable for prod-like performance testing
- `dev-sh-prof-*.project.local` - Various profiling modes (see below)



Because we can only gives us `--project-file` to select configuration, we need
each of these local overrides to have a symlink back to the top-level
`cabal.project`. So e.g. if you wanted to build with CI settings locally you
would do:

    $ cabal build --project-file=cabal/ci.project exe:graphql-engine

Likewise for the freeze file symlinks.

In cabal-install 3.8 we can have `import`s, which we also use.

Here's a helper for making new configurations:

```
function hasura_new_sub_config () {
    cd "$(git rev-parse --show-toplevel)/cabal"
    ln -s ../cabal.project.freeze "$1.project.freeze"
    ln -s ../cabal.project "$1.project"
    touch "$1.project.local"
    echo "continue editing: $1.project.local"
    cd - &>/dev/null
}
```

-------------------------------------------------------------------

## Profiling modes

See the `graphql-engine --prof-*` flags in `dev.sh` for the happy path to use these modes.

### `+RTS -hi`(info map) Heap Profiles

Every distinct constructure allocation site is instrumented with source code
information

See: `dev-sh-prof-heap-infomap.project.local`

- **Try it when**: you want to go deeper debugging high resident memory during development 
- **Benefits**: doesn't inhibit optimizations, very granular
- **Downsides**: must recompile, binary sizes can get large, sometimes source info is confusing

### ghc-debug

A set of client and server libraries for snapshotting and arbitrarily analyzing
the Haskell heap

- **Try it when**: you need to answer any sort of complex question about what's in memory; e.g.
  why is something retained? do we have many identical strings and memory?
- **Benefits**: extremely powerful, can run on production without restart
- **Downsides**: analysis passes can take time to write and debug, analyzing large heaps requires care

### “Ticky ticky” profiling

Generates a report for all allocations even those that are very short-lived;
quasi-  time profiling

See: `dev-sh-prof-ticky.project.local`

- **Try it when**: debugging a regression and bytes allocated, comparing two different versions of code 
- **Benefits**: see and compare allocations directly,  doesn't inhibit optimizations
- **Downsides**:  STG can take time to decipher, the program gets very slow, not suitable for production

### `-fprof-late` time profiling

**NOT YET IMPLEMENTED**

Time profiling that instruments code after all significant optimizations have
been performed, so it doesn't distort the profile (on 9.4+ only, but plug-in
available)

See: `dev-sh-prof-time.project.local`

- **Try it when**: you want to try to make some code faster, or understand where the time is being spent in the system
- **Benefits**: get call stacks, granular view of execution time
- **Downsides**: requires recompilation, STG can be confusing, not suitable for production
