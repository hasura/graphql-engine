# Event triggers in MS-SQL

This document outlines research for supporting event triggers in a MS-SQL database. This RFC includes only the event triggers support from a
Database PoV, which can be divided in two parts as the following:

## Generating new events

1. For supporting event triggers, we need to generate new events on a mutation, whether
   the mutation is done through Hasura or not. This can be done via an DML SQL triggers which are
   supported by the [MS-SQL triggers](https://docs.microsoft.com/en-us/sql/t-sql/statements/create-trigger-transact-sql?view=sql-server-ver15.).

2. An MS-SQL trigger is different from a postgres trigger in some ways
   * MS-SQL doesn't support triggers which trigger for each row, so in case of mutations which affect multiple rows, there'll only be a single
      trigger fired which will contain the data of all the rows that were affected.
   * MS-SQL maintains two logical tables, namely, [`inserted` and `deleted`](https://docs.microsoft.com/en-us/sql/relational-databases/triggers/use-the-inserted-and-deleted-tables?view=sql-server-ver15).
      The rows in the `inserted` table are copies of the new rows in the trigger table and similarly the `deleted` table contains the
      copies of the rows that were deleted from the trigger table.
   * When there's an update transaction, the old data (before the update) will be copied to the `deleted` table and the new data will be copied
      to the `inserted` table.

3. The `data` value of the event trigger's payload should be as following:

   ```json
   {
     "data": {
       "old": <column-values>,
       "new": <column-values>
     }
   }
   ```

   In postgres, we could use the `row_to_json` function to convert a table row into JSON. In SQL server, the way to convert an SQL row into
   JSON is different. Say, we have a table `authors`, which has three columns - `id`, `name` and `created_at`. We can convert the rows
   of the table into JSON by performing the following query:

   ```sql
      select * from authors FOR JSON PATH;
   ```

   which will return a single row which will contain an JSON array with the rows info formatted into JSON with the column names being the
   keys of the JSON object and the value being the value of those keys. For example,

   ```json
   [
     {
       "id": 1,
       "name": "author 1"
     },
     {
       "id": 2,
       "name": "author 2"
     }
   ]
   ```

4. Insert and delete event triggers are easier than the `update` triggers, because in the former, we only need to format the data
   present in the `inserted` and `deleted` tables and insert it into the `hdb_catalog.event_log` table. For updates, it's not so
   straight-forward because we need to combine data from the `inserted` and `deleted` tables to construct the payload as mentioned
   in #3.

   So, the following is proposed for `UPDATE` triggers:

   ```sql
      CREATE TRIGGER hasuraAuthorsAfterUpdate
      ON authors
      AFTER UPDATE
      AS
      BEGIN
      DECLARE @json NVARCHAR(MAX)
      SET @json =  (
        select d.id as [data.old.id], d.name as [data.old.name], i.id as [data.new.id], i.name as [data.new.name]
        from deleted d
        JOIN inserted i on i.id = d.id # join using the primary key of the table
        where (i.id != d.id || i.name != d.name) # check if there has been a value change
        FOR JSON PATH
      )
      insert into hdb_catalog.event_log (payload)
      select value from OPENJSON (@json)
      END
   ```

   NOTE: the above will work only when a table has a primary key, which is used to join the `deleted` and the `inserted` tables.

   The triggers will be created with template string values where the values of the tables or row expressions will be substituted
   before creating the trigger, as it is done for postgres [here](https://github.com/hasura/graphql-engine-mono/blob/main/server/src-rsr/trigger.sql.shakespeare).

5. MS-SQL doesn't allow for the trigger to be created in a different schema from the target table's schema.
   For example, if a table is created in the `dbo` schema, then the trigger should also be in the `dbo` schema.

6. In postgres, the session variables and trace context were set in runtime configurations, `hasura.user` and `hasura.tracecontext` respectively, it's done by setting these values via `SET LOCAL \"hasura.user\"={\"x-hasura-user-id\":\"1\"}`. In MS-SQL, the same can be done using [SESSION_CONTEXT](https://docs.microsoft.com/en-us/sql/t-sql/functions/session-context-transact-sql?view=sql-server-ver15).

   There are some differences between the postgres and MS-SQL session contexts,

   * In postgres, there's an option to localize the session context only to a transaction (using `SET LOCAL`), but there's no way to do the
     same in MS-SQL. In MS-SQL, the session context will be set for the whole context. So, for this to work in MS-SQL, we should only have
     one transaction per session (which already exists).

7. The aim is to do as little work as possible in the source DB i.e. the source should only capture the `new`,`old`, `operation_type`, `session_variables` and `tracecontext` in an event log, the JSON processing of these details will be done by the graphql-engine during
   the delivery of the event.

## Fetching pending events

1. MS-SQL doesn't support a `JSON` column type and instead is stored in a column with `NVARCHAR(MAX)` type. So, we can't rely on the database that the
   value in the `payload` will be an valid JSON value. MS-SQL does provide a function [ISJSON](https://docs.microsoft.com/en-us/sql/t-sql/functions/isjson-transact-sql?view=sql-server-ver15) which can be used to check if a value is valid JSON.

2. As we know, there can be multiple instances of hasura running on the same source/database. So, we need to make sure that the multiple instances
   do not fetch the same rows, otherwise the same events will be processed more than once. To solve this problem, postgres uses the `FOR UPDATE SKIP LOCKED` which when used in a `SELECT` query will skip over the rows that are locked by other transactions **without waiting**.

   MS-SQL has a similar feature, [READPAST](https://docs.microsoft.com/en-us/sql/t-sql/queries/hints-transact-sql-table?view=sql-server-ver15) which is more or less like `FOR UPDATE SKIP LOCKED`. From the docs,

   > READPAST is primarily used to reduce locking contention when implementing a work queue that uses a SQL Server table. A queue reader that uses READPAST skips past queue entries locked by other transactions to the next available queue entry, without having to wait until the other transactions release their locks.

### Server code changes

1. Support source migrations for MS-SQL sources, which will create the `event_log` and the `event_invocation_logs` table.

2. Currently, events processing can be broken up into two steps:

   1. Fetching the events from the database.
   2. Processing the fetched events.

   The current events processing code is postgres specific, this will need to change to be for any
   backend `b`, like we have done with the `BackendMetadata` type class. The type class proposed
   here will be `BackendEventTrigger`, which will be defined in the following way:

   ```haskell

      class (Backend b) => BackendEventTrigger (b :: BackendType) where

         -- events are fetched per source
         fetchEvents
           :: MonadError QErr m
           => SourceName
           -> Int -- ^ events batch size
           -> m [Event]

         insertEventLogInvocation
           :: MonadError QErr m
           => Invocation 'EventType
           -> m ()
   ```

   By defining the above typeclass, in the future new backends can be easily added just by implementing
   the `BackendEventTrigger` instance for those backends.

3. The creation of event triggers in the current code is generalized for all backends, so the error placeholders will be
   needed to replace with appropriate backend-specific logic.

## Blockers

1. At the time of writing this RFC, mutations aren't yet supported in MS-SQL. Support for mutations is needed to set the
   `session_variables` and the `trace_context` in the database. This is not a hard blocker though, this can be added incrementally
   after support for mutations is added.
