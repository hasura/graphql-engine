# hasura-edit-pg-dump

This is a serverless function written in Go and deployed onto now.sh which can
take the SQL output from `pg_dump` and clean it up so that it can be used as a
migration file with Hasura.

1. SQL front matter, like `SET` statements are removed.
2. `CREATE SCHEMA public` is removed.
3. Comments (`--`) and empty newlines are removed.
4. Postgres triggers created by Hasura for event triggers are removed.

This app is available at https://hasura-edit-pg-dump.now.sh

Usage:

```bash
curl --data-binary @file.sql https://hasura-edit-pg-dump.now.sh > cleaned.sql
```

A bash version is also available:

```bash
./edit-pg-dump.sh filename.sql
```
