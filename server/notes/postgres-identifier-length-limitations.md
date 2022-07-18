This note is in [Hasura.Backends.Postgres.SQL.IdentifierUniqueness](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/SQL/IdentifierUniqueness.hs#L22).
It is referenced at:
  - line 9 of [Hasura.Backends.Postgres.SQL.IdentifierUniqueness](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/SQL/IdentifierUniqueness.hs#L9)
  - line 33 of [Hasura.Backends.Postgres.SQL.IdentifierUniqueness](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/SQL/IdentifierUniqueness.hs#L33)

# Postgres identifier length limitations

Postgres truncates identifiers to a maximum of 63 characters by default (see
https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS).

