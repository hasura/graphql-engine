# Managed resources

## Motivation

Most resources need to be explicitly freed after being acquired. In most programming languages,
there's an idiomatic way of ensuring that this is done properly, even in the case of an error. C++
famously uses [RAII](https://en.cppreference.com/w/cpp/language/raii) (Resource Acquisition Is
Initialization), where the constructor of a type acquires the resource and its destructor releases
it. Python uses the [`with`
statement](https://docs.python.org/3/reference/compound_stmts.html#the-with-statement).

In Haskell, an idiomatic way of doing this is with `withX` functions, that take a callback as an
argument, such as
[`withFile`](https://hackage.haskell.org/package/base-4.18.0.0/docs/System-IO.html#v:withFile): the
function acquires the resource, calls the associated computation, and frees the resource.

```haskell
withFoo :: FooArgs -> (Foo -> IO a) -> IO a
withFoo args callback = do
  foo <- acquireFoo args
  callback foo `finally` releaseFoo foo
  -- or: bracket (acquireFoo args) releaseFoo callback
```

However, this becomes quickly unwieldy when trying to acquire several resources, like we do in the
initialization of the engine:

```haskell
main = do
  withLoggers \loggers -> do
    withPGPool \pgPool -> do
      withMSSQLPool \mssqlPool -> do
        withMetadataConnection \metadataDB -> do
          runEngine loggers pgPool mssqlPool metadataDB
```

But an astute reader might have noticed that this is a staircase pattern in which each line just
_binds_ a newly acquired resource to a name... This is a monad!

## `Managed`

The `Managed` monad is defined in
[`Control.Monad.Managed`](https://hackage.haskell.org/package/managed-1.0.9/docs/Control-Monad-Managed.html). It
is defined (roughly) as such:

```haskell
newtype Managed a = Managed (forall r . (a -> IO r) -> IO r)
```

With it, the above block of code can be rewritten as:

```haskell
main = runManaged do
  loggers    <- managed withLoggers
  pgPool     <- managed withPGPool
  mssqlPool  <- managed withMSSQLPool
  metadataDB <- managed withMetadataConnection
  runEngine loggers pgPool mssqlPool metadataDB
```

In theory, we could run our entire engine in `Managed` instead of `IO`, since `Managed` provides
`MonadIO`, and we'd be able to acquire all of our resources easily! However...

## Limitations

`Managed` has two major limitations that prevent us from easily doing this. First and foremost: it
is limited to `IO` callbacks. If we happen to be operating in `ReaderT HandlerCtx (ReaderT AppEnv
Managed)`, we can't use a `with` function that operates in that same monad: the definition of
[`managed`](https://hackage.haskell.org/package/managed-1.0.9/docs/Control-Monad-Managed.html#v:managed)
restricts us to `IO` callbacks.

This is, of course, the reason why `MonadBaseControl IO` and `MonadUnliftIO` exist: they provide the
ability to un-lift and re-lift computations, to allow `IO` callbacks. But, and that's the second
thorn in our side, `Managed` does not provide an instance of either of those two classes.

This proves to be a major blocker for us:
  - we *do* rely on being able to give callbacks in complex monads
  - we *do* rely on having access to `MonadBaseControl IO` for some of the libraries we use

Hence the need for something a bit more sophisticated.

## `ManagedT`

This is why our code contains a _transformer_ definition of `Managed`:
[`ManagedT`](https://github.com/hasura/graphql-engine/blob/main/server/src-lib/Control/Monad/Trans/Managed.hs). It
is defined (roughly) as such:

```haskell
newtype ManagedT m a = ManagedT (forall r. (a -> m r) -> m r)
```

Instead of being limited to `IO`, this version allows us to allocate resources in the underlying
monad, whichever it might be. While it still doesn't give us a `MonadBaseControl IO` instance, we
can still run everything in the underlying monad, that is more complex than `IO`, and lift the
result back up to `ManagedT`. For instance, in the initialization, we do something akin to the
following:

```haskell
main = do
  appEnv <- getAppEnv
  runAppM appEnv do
    -- this block is in `AppM`, which is on top of `IO`
    lowerManagedT do
      -- this block is in `ManagedT AppM`
      foo <- allocate acquireFoo releaseFoo
      -- we run the engine in the original `AppM`, *without ManagedT*
      lift $ runEngine foo
```

This is a practical compromise: our resources are still handed by a clean monadic construct, but
since that monad sits at the top of the stack, we can run our actual computations in the underlying
stack and its full complexity, without being limited to `IO`.

The only major downside is that we have to stick a `ManagedT` at the top of the stack whenever we
need to resource allocation, and at time of writing we have to do that *twice* in the initialisation
code of the engine. But that's a small price to pay.

## An additional motivating example

The best example of how we make use of `ManagedT` can be found in
[`Control.Concurrent.Extended`](https://github.com/hasura/graphql-engine/blob/main/server/src-lib/Control/Concurrent/Extended.hs). This
small internal library provides a wrapper around
[`Control.Immortal`](https://hackage.haskell.org/package/immortal-0.2.2.1/docs/Control-Immortal.html)
that allows us to use the `ThreadId` as an acquired resource and to gracefully shutdown the immortal
respawning thread when the surrounding `ManagedT` context ends. And, due to using `ManagedT` at the
top of the stack, we can spawn threads in any underlying monad without having to do any manual
unlifting / lifting. See for instance `forkManagedT`, which is defined (more or less) as follows:

```haskell
forkManagedT
  :: MonadBaseControl IO m
  => m Void
  -> ManagedT m Immortal.Thread
```

The new immortal thread will run an action in `m`, and the surrounding `ManagedT` context will
manage the lifetime of the `threadId`: when it exits, `Control.Immortal.stop` will be called on the
thread.
