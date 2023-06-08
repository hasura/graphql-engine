
# State of Hedis and TODOs

In order of descending priority.


## Github Issues and Pull requests

* Priorities vary
* Some of them overlap with the big-picture issues discussed below.


## Redis API Completeness

The idea of Hedis is to support all Redis commands by having one typesafe
function per command.

* Commands code generator:
  The code generator uses the commands.json file from
  github.com/antirez/redis-doc which is used on the redis.io website.
  - My current state of mind is to completely remove the code generator and
    implement all commands by hand. Otherwise it has to be support more/all
    commands.
  - PROs of the code generator
    - Can be sure that all commands exist as Haskell functions
    - Automatic Haddock with one-sentence description and link to Redis
      documentation website.
    - No errors/mistakes where it works
  - CONs of the code generator
    - It's a mess, partly because commands.json the Redis API is a mess itself.
    - Is not "feature-complete", some commands have/need manual implementations.
    - Does not support optional arguments very well.
    - commands.json does not document all return types -> they need maintenance.

* Optional Arguments
  Currently has more than one function per Redis command for different optional
  arguments (e.g. BITOP).
  - Only have one function per command that takes an "optionals" argument. See
    the `OPTS a` monoid in redis-resp http://hackage.haskell.org/package/redis-resp-0.3.2/docs/Data-Redis-Command.html#t:Opts
    for a great solution. (Although it's not quite perfect since it is possible
    to add conflicting optionals args, e.g. use both XX and NX in the SET
    command.)
  - Might still be necessary to have multiple functions when optional arguments
    change the return type (e.g. ZRANGEBYSCORE). Or only allow one return type
    (e.g. always use ZRANGEBYSCOREWITHSCORES).

* Hedis supports commands up to Redis 2.6.?. The newer commands need
  implementations (decide on the fate of the code generator first).

* Mapping of Haskell types to Redis types: simplicity and convenience vs.
  correctness (Some issues and pull requests on this topic).
  1. All containers are Haskell lists (current approach):
    - nice list syntax.
    - easy conversion to all collection types.
    - No real loss of safety for return types
    - can fail for arguments (list vs. non-empty)
  2. "correct" container types for each Redis type:
    - Non-empty lists (unconvenient syntax?)
    - Sets
      - Which set type, Set vs HashSet vs ..
      - Redis already returns "sets" in the sense that each element exists only
        once in the returned list.
    - Maps
    - etc.


## Command Return Types

Currently every command returns the result wrapped in `Maybe Reply`. This is
"what Redis returns" but it's unconvenient to use.

* Return the results "unwrapped" and throw an exception when the reply is an
  error or can not be decoded to the desired return type.
* Does this work with the `Queued` return type in Mult/Exec?
