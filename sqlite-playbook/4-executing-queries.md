# Step 4: Executing queries

Now that we're able to translate queries from the GraphQL IR into SQLite SQL,
we need to also execute it.

That is the job of the `BackendExecute` and `BackendTransport` type classes.

The methods in `BackendExecute` are responsible for building the monadic
actions that will execute the queries when performed.

[Hasura.Backends.SQLite.Instances.Execute](../server/src-lib/Hasura/Backends/SQLite/Instances/Execute.hs).

The methods in `BackendTransport` are then responsible for actually running the
actions and performing logging.

For our purposes the distinction is mostly irrelevant, as `ExecutionMonad 'SQLite = IO`.

[Hasura.Backends.SQLite.Instances.Transport](../server/src-lib/Hasura/Backends/SQLite/Instances/Transport.hs).

Note that this particular part of the Backend API is very much in flux and
likely to change in upcoming releases.

Next up is [Step 5: Defining the schema](5-defining-schema.md)
