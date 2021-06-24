# Step 2: Sqlite metadata

Now the GraphQL-Engine needs to be able to know what objects exist in our SQLite source.

HGE is able to store a large variety of metadata about database objects, but
we'll just care about recording what tables exist and what coulmns they have
for now.

An SQL expression for this is given in [sqlite_table_metadata.sql](../server/src-rsr/sqlite_table_metadata.sql)

We will need to define `resolveDatabaseMetadata` in [Hasura.Backends.SQLite.Instances.Metadata](../server/src-lib/Hasura/Backends/SQLite/Instances/Metadata.hs).

Next up is [Step 3: Translating queries](3-translating.md).

