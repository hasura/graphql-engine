Several mostly separate concerns:

1. How do we interrupt graphql-engine PostgreSQL client code while executing commands.
   (async + postgres cancel, switch to non-blocking libpq, FFI "interruptible", ...)
2. What should communication between graphql-engine and PostgreSQL server be
   when interrupting. (close connection, cancel, just let it finish, ...)
3. What should the externally observable behaviour be, to someone talking to both
   graphql-engine and datasources (PostgreSQL server) as a client.
   (guarantee that we communicate precisely whether a command was executed absent
   networking issues?)
4. How do we time out operations inside graphql-engine from a high-level point of view?
   (top-level `System.Timeout.timeout`, lower-level `timeout`, check an mvar, exception
   safety, ...)


[rob]: This is a bit of a brain-dump, currently. I'd particularly like input on:
- (1.) The async/wait/catch/cancel solution seems workable to me. I'm a bit vague on
  some details, but I feel it should be possible to make this reliable and correct.
  But again I'm a bit inexperienced with the async runtime, FFI, so might be missing
  things.
- (1.) I can dig further into non-blocking libpq, but feel it's not worth the effort
  at this stage.
- (2.) Least unsure about this, it seems clear that cancelling commands is the way to
  go. But do object :)
- (3./4.) "timeout -> send cancel -> wait for response and use regular error path"
  seems cleanest, but am a bit vague
- (4.) Is the top-level use of `timeout` safe enough? I have vague doubts here, lacking
  familiarity with the code base and experience working with asynchronous exceptions.


## 1. Interrupting PostgreSQL command execution

The FFI calls to [PGexec*](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-EXEC-MAIN)
block the RTS, so a priori asynchronous exceptions won't be handled until after the
server responds.

(This point interacts closely with 2. below; i.e., simply making the calls interruptible
might not be enough for a correct solution towards PostgreSQL server.)

### Interruptible FFI (doesn't work)

Marking the FFI call [interruptible](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ffi.html#extension-InterruptibleFFI)
should cause the OS-level thread to receive SIGPIPE on async exceptions, which would cause
system calls to fail with EINTR. However, as verified experimentally, it appears this doesn't
cause libpq to return, likely because it retries on EINTR (compare https://doxygen.postgresql.org/fe-misc_8c.html#a03fea567926b20bbeada40c134a8fdc1).

### Call libpq using `async` (separate thread)

We can make FFI actions interruptible towards the rest of the program by running them
in a separate thread:

```haskell
do
  a <- async $ uninterruptibleFFI conn
  wait a
```

While this allows interrupting the action, it isn't thread-safe as such due to
handing over the libpq connection to another thread. What we can do is to cancel
the command (see 2. below) and then wait for the call to return:

```haskell
do
  c <- getCancel conn
  a <- async $ uninterruptibleFFI conn
  res <- try $ wait a
  case res of
    Left e -> do
      cancel c
      throwTo (asyncThreadId a) e
      wait a
    Right x -> return x
```

Couple things to figure out:
- We might want to swallow certain exceptions (e.g. `System.Timeout.Timeout`
  or a custom `PGCancelCommand`), in which case we wouldn't rethrow the exception
  and instead just return regularly (with a postgres "canceled" error message if
  we canceled on time) -- this would allow user code to decide whether the command
  was actually interrupted. (See 3. below.)
- Error handling for the call to `cancel`.


### Use libpq in non-blocking mode

We could rewrite parts of pg-client-hs to use libpq
[asynchronously](https://hackage.haskell.org/package/postgresql-libpq-0.9.4.3/docs/Database-PostgreSQL-LibPQ.html#g:9).
The general approach would be:

- get the connection socket file descriptor via `socket :: Connection -> IO (Maybe Fd)`
- loop with `poll()/select()` and `consumeInput`
  ([rob]: is this in the standard library somewhere? this should be interruptible)

[rob]: I haven't explored this option in detail since it seems like the change
  has a far bigger impact, and the forking approach seems like an easier way to
  explore this. May well be a good idea longer term though.


## 2. Interrupting towards PostgreSQL

There's a number of things that our postgresql client could do when interrupted:
- closing or resetting the connection
- sending a cancel message
- process the command normally (i.e., wait and read the result, from the connection)

Breaking the connection won't interrupt command execution server-side, it seems
cancelling is generally the right thing to do:
- https://www.postgresql.org/message-id/flat/CA%2BTgmobakAdqELHtB_t8mPkU8qn4QPSJuCrRzWJFGfCJtpoV%3Dw%40mail.gmail.com
- https://dba.stackexchange.com/questions/81408/is-a-postgres-long-running-query-aborted-if-the-connection-is-lost-broken


## 3. From the outside

Let's say we have a slow graphql mutation that translates to a PostgreSQL transaction.
It seems desirable to guarantee that we respond with:

- no error: the transaction was committed
- controlled timeout: the transaction was not committed
- other errors: case by case, generally same behaviour as without timeouts, plus
  new failure cases with e.g. an error sending the cancel message

The idea would be that if there are no connection issues, the client can reliably
tell whether a mutation happened by looking at the result. This is possible to achieve
with cancellation, by ensuring that after we send the cancel message, we continue the
regular control flow, which would then return a PostgresQL "command cancelled" error
if the command was cancelled, and the usual response otherwise. (E.g. in the case that
we send the cancel around the same time as the server sends the successful response.)

[rob]: I lack knowledge here to what extent we try to guarantee anything reliable at
  all with respect to transactionality towards Postgres. It's clear that we can't
  provide similar guarantees e.g. towards remote schemas.


## 4. High-level graphql-engine implementation

- Execute the operation under `System.Timeout.timeout` at the top level (`runGQL`).
  [rob]: I'm unsure to what extent we're prepared to be interrupted randomly, e.g.
    are we safe against things like `r <- executePostgresQuery; logResult; return r`
    being interrupted before the logging step (and less harmless similar scenarios).
- Execute slow operations under `System.Timeout.timeout` at a lower level. E.g.
  we might set a deadline, and pass it in, and then use `System.Timeout.timeout`
  within `pg-client-hs` or different client code to ensure we don't pass that deadline.
- For either of the above cases, we might opt to throw a different asynchronous
  exception at the operation (`HasuraCancel`).
- Instead of returning `Nothing` from `timeout`, the interrupting exception could
  result in an "interrupted" error being returned along the usual error return path.
- Instead of throwing an asynchronous exception, we could pass the "interrupted"
  information via a different mechanism, e.g. an MVar. E.g. non-blocking postgres
  code could wait on both the timed-out mvar and the connection socket.
