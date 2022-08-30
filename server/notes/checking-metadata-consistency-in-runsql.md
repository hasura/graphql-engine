This note is in [Hasura.Backends.Postgres.DDL.RunSQL](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/DDL/RunSQL.hs#L123).
It is referenced at:
  - line 102 of [Hasura.Backends.Postgres.DDL.RunSQL](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/DDL/RunSQL.hs#L102)
  - line 204 of [Hasura.Backends.Postgres.DDL.RunSQL](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/DDL/RunSQL.hs#L204)

# Checking metadata consistency in run_sql

SQL queries executed by run_sql may change the Postgres schema in arbitrary
ways. We attempt to automatically update the metadata to reflect those changes
as much as possible---for example, if a table is renamed, we want to update the
metadata to track the table under its new name instead of its old one. This
schema diffing (plus some integrity checking) is handled by withMetadataCheck.

But this process has overhead---it involves reloading the metadata, diffing it,
and rebuilding the schema cache---so we don’t want to do it if it isn’t
necessary. The user can explicitly disable the check via the
check_metadata_consistency option, and we also skip it if the current
transaction is in READ ONLY mode, since the schema can’t be modified in that
case, anyway.

However, even if neither read_only or check_metadata_consistency is passed, lots
of queries may not modify the schema at all. As a (fairly stupid) heuristic, we
check if the query contains any keywords for DDL operations, and if not, we skip
the metadata check as well.
