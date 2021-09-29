# Overview

We want to be able to time out slow queries and mutations.

Concretely specced (issue [#1232](https://github.com/hasura/graphql-engine-mono/issues/1232)):
- `api_limits` in metadata gets an extra `operation_timeout` setting
  (next to `rate_limit`, `depth_limit`, `node_limit`), configurable
  project-wide and per user
- any GraphQL operation should be aborted once the timeout is reached;
  this includes REST endpoints via their mapping to GraphQL operations,
  and subscriptions.

This RFC outlines a minimal solution, and tries to provide an
overview of the design space, mostly through several appendices
that can be ignored when evaluating the concrete plan. There's
a bit more detail in
[operation-timeout-api-limits-braindump](./operation-timeout-api-limits-braindump.md).

There's a rather wide design space here, some questions,
the answers to which determine how we go about this:
1. Do we let the client know whether a query was actually interrupted
   database-server-side? Compare Appendix C.
2. Do we need queries to timeout reliably when there
   are connectivity issues between graphql-engine and the
   database servers?
3. Some considerations regarding the the effect of the approach
   on stability, maintainability, understandability of the code
   base, e.g.:
   - Is the core query code robust with respect to exception
     handling? Do we want to afford the effort of ironing out
     bugs there? Is the behavioural complexity of using asynchronous
     exceptions for high-level control flow a good trade-off
     compared to the implementation complexity of more explicit
     timeout handling?
   - Is the effort to rewrite the postgres client library in
     non-blocking mode worthwhile?

For the first implementation, we've generally opted for choices
which require the smallest changes. None of these choices prevent
us from reevaluating in the future. Concretely:

- Answer "no" to points 1 & 2; this allows using `timeout` (compare
  Appendix B on library API design), and a minimal change
  to `pg-client-hs` (compare Appendix A on pg-client-hs).
- Answer "yes / we'll see" to the robustness question, and leave
  the potential non-blocking rewrite to the future.

# Implementation

There are a couple of mostly-independent things to do:

- modify backend library code to allow interrupting queries
  (concretely, `pg-client-hs`)
- provide hooks in the server query execution to allow us to
  apply limits
- extend the metadata to allow configuring timeouts
- implement hooks for Pro referencing the metadata

## Concrete technical plan

- `ApiLimits` in metadata gets a new `operation_timeout` field, analogous
  to other limits
- Postgres backend library `pg-client-hs` is modified so that queries
  are interruptible by async exceptions (absent networking issues).
  This is PR [#39](https://github.com/hasura/pg-client-hs/pull/39).
- class `HasResourceLimits` (which currently has `askResourceLimits`
  yielding an HTTP handler allocation limiter) is augmented to
  offer `askHandlerLimits` and `askOperationLimits`
- `askOperationLimits` returns an action modifier (`m a -> m a`)
- for Pro, we implement this action modifier to call
  `Timeout.timeout :: IO a -> IO (Maybe a)` on the action, and
  throw an error if the timeout triggered
- for both HTTP (in `runGQ`) and Websocket transport (in `onStart`),
  we modify query and mutation execution with this operation limiter

# Open questions, random thoughts

- How to handle subscriptions concretely?
- By the placement of the limiter, we're making a choice about what
  concretely is part of the time-limited operation (e.g., do we
  limit "cache lookup + execution", or just "execution"; should
  request parsing count?)
- The graphql-engine change will affect all backends, but this RFC
  doesn't cover potentially required changes analogous to the
  `pg-client-hs` changes. Without those, long-running queries to
  other backends would still fail, but might not return promptly
  (and won't save resources).
- Unit for timeout is seconds (but can we make that clearer? e.g.
  operation_timeout_seconds or `operation_timeout: 5s`)
- Offering operation limits for cloud ops (SRE), as opposed to
  project administrators.

# Appendix A: (Un-)Blocking postgres queries

The current implementation of `pg-client-hs` executes queries
synchronously in FFI, and doesn't provide a timeout mechanism.
When receiving an asynchronous exception, the action will block
until a response is received from the server. There are several
ways to go about this. (Compare also Appendix B on the library
API below.)

- `pg-client-hs` builds on the C library `libpq`, which is built
  in a way that doesn't allow us to use "interruptible"
  FFI, because it catches signals itself
- for the query to be interrupted on the database server,
  we need to send a cancel message:
  - [psql-hackers message](https://www.postgresql.org/message-id/flat/CA%2BTgmobakAdqELHtB_t8mPkU8qn4QPSJuCrRzWJFGfCJtpoV%3Dw%40mail.gmail.com)
  - [stackoverflow question](https://dba.stackexchange.com/questions/81408/is-a-postgres-long-running-query-aborted-if-the-connection-is-lost-broken)
- options:
  1. (use a native haskell postgres client)
  2. (patch libpq to allow using it with interruptible FFI)
  3. use libpq in non-blocking mode (and `select(2)` on the file
     descriptor)
  4. use libpq in non-blocking mode and busy-wait
  5. cancel queries in blocking mode

# Appendix B: Timeout API design for a database client library

Say you're designing a Haskell database client library, e.g.
`pg-client-hs`, and would like to allow library users to
timeout queries. There's a range of approaches to how to
provide this functionality:

1. query is essentially of the type
  `query :: Query -> IO Result`;
  we just ask that `query q` respects async exceptions
  (i.e., bails out cleanly and promptly when receiving an async exception,
  throwing that exception)
  client code would be expected to use `timeout :: DiffTime -> IO a -> IO (Maybe a)`
2. query is essentially of the type
  `query :: Query -> IO (Either TimeoutError Result)`;
  if `query q` receives (a specific / any) async exception, it bails out
  cleanly, swallowing the exception and returning `Left TimeoutError`
  client code would use a variant of `timeout` with a custom exception, that doesn't
  need to decide whether the timeout was handled (`timeout' :: DiffTime -> IO a -> IO a`)
3. query is essentially of the type
  `query :: TimeLimit -> Query -> IO (Either TimeoutError Result)`;
  timeouts are handled internally in some way that the library user doesn't
  care about
  - option: Deadline vs. TimeLimit
  - async exception behaviour should ideally be as in option 1 above, but
    e.g. blocking in FFI code would be fine
  client code just needs to pass a time limit / deadline
4. query is essentially of the type
  `query :: Cancel -> Query -> IO (Either CancelError Result)`
  where `Cancel` is some channel to allow signalling the desire of client code to
  interrupt a query (e.g. `MVar ()`); library code would check for cancellation
  at appropriate spots

Some trade-offs:
- options 2 and 4 offer most flexibility to the library user
- option 2 violates the core contract of async exceptions, namely "stop what you're
  doing and bail, rethrowing the exception"
- option 1 doesn't allow the library user to determine whether the query was interrupted
  server-side (i.e., there's no way to decide between "timeout hit while query was running
  and caused the remote transaction to be rolled back" and "timeout hit while client query
  code was still running, but remote transaction committed")

# Appendix C: Behaviour towards graphql clients

For backends that allow reliably deciding whether a
transaction committed, do we provide this to the client?
I.e., we could guarantee the following.
Assuming a graphql query response is received client-side,
then either:
- there's no error and the query was executed successfully
- there's an error of a class "all bets are off" (e.g.,
  network error talking to the database backend)
- there's an error of a class "database query not performed" (e.g.,
  "malformed graphql", "table doesn't exist", "server shut down
  while running query"); and if our new timeout triggers, it will
  cause the transaction to not be committed, and reported via this
  class
