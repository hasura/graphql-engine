This note is in [Hasura.Backends.Postgres.SQL.Rewrite](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/SQL/Rewrite.hs#L12).
It is referenced at:
  - line 19 of [Hasura.Backends.Postgres.SQL.Rewrite](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/SQL/Rewrite.hs#L19)

# Postgres identifier length limitations

Postgres truncates identifiers to a maximum of 63 characters by default (see
https://www.postgresql.org/docs/12/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS).

