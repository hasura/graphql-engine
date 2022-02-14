# Logging database stats during source catalog migration

## Metadata

```
---
authors:
  Karthikeyan Chinnakonda <karthikeyan@hasura.io>
state: In review
---
```
## Description

Sometimes we need to change the alter some structure in the source catalog, like adding a new column, transforming an existing column's type to another etc. The problem many users have faced during migrations is that sometimes the DDL
queries run during the migration lock the database and as a result, the graphql-engine doesn't start up. When this happens
there are no logs outputted by the graphql-engine and there's no way for the user to know what's happening and this is frustrating for the user.

## Proposed solutions

1. When the source catalog migration is being done, we can log the output of the following SQL query every five seconds till the migration completes:

```sql
SELECT json_agg(json_build_object('query', psa.query, 'lock_granted', pl.granted, 'lock_mode', pl.mode, 'transaction_start_time', psa.xact_start, 'query_start_time', psa.query_start, 'wait_event_type', psa.wait_event_type, 'blocking_query', SUBSTRING(blocking.query, 1, 20) ) order BY psa.query_start)
FROM     pg_stat_activity psa
JOIN     pg_stat_activity blocking ON blocking.pid = ANY(pg_blocking_pids(psa.pid))
LEFT JOIN pg_locks pl ON psa.pid = pl.pid
WHERE    psa.query LIKE '%hdb_catalog%' AND psa.wait_event_type IS NOT NULL AND psa.query ilike any (array ['%create%', '%drop%', '%alter%']);
```

The above query only looks for DDL changes made in the `hdb_catalog` schema. Only 20 characters of the blocking query is outputted because it may contain
sensitive data.

The output of the above query looks like the following:

```json
[
  {
    "query": "INSERT INTO authors (name) values ('boo');",
    "lock_granted": true,
    "lock_mode": "ExclusiveLock",
    "transaction_start_time": "2021-10-19T12:08:14.903342+00:00",
    "query_start_time": "2021-10-19T12:08:14.903342+00:00",
    "wait_event_type": "Lock",
    "blocking_query": "LOCK authors IN ACCESS EXCLUSIVE MODE;"
  },
  {
    "query": "INSERT INTO authors (name) values ('boo');",
    "lock_granted": false,
    "lock_mode": "RowExclusiveLock",
    "transaction_start_time": "2021-10-19T12:08:14.903342+00:00",
    "query_start_time": "2021-10-19T12:08:14.903342+00:00",
    "wait_event_type": "Lock",
    "blocking_query": "LOCK authors IN ACCESS EXCLUSIVE MODE;"
  }
]
```

Doing this will let the user know that some of the queries that are running on the database are in a locked state.

##  Implementation details

1. We'll create a new function `logPGSourceCatalogStats` which will infinitely query in fixed intervals of five seconds, the database using the above mentioned SQL query. The type signature of the function will look like:

`logPGSourceCatalogStats :: forall pgKind m . (MonadIO m, MonadTx m) => Logger Hasura -> SourceConfig '(Postgres pgKind) -> m Void`

2. The `logPGSourceCatalogStats` function will be run in a separate thread so as to not block the migrations. This can be done by  calling `logPGSourceCatalogStats` using the `forkManagedT` function.

3. To avoid implementing this feature for metadata DB catalog migrations, we need to make the 40_to_41 migration a no-op and move the logic to
   be a source catalog migration. Which means to make the `40_to_41.sql` a noop migration and move the migration to `0_to_1.sql` in `pg_source_migrations`. The `41_to_40.sql` migration should change accordingly, it has to check first whether the `locked` column is a `timestamp with time zone` and iff alter it to the boolean type otherwise do nothing.


## Open questions

1. What will the user do from the above info to resolve the locking? Is the logging merely informational?
