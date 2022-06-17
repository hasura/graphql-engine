This note is in [Hasura.Backends.Postgres.SQL.IdentifierUniqueness](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/SQL/IdentifierUniqueness.hs#L25).
It is referenced at:
  - line 9 of [Hasura.Backends.Postgres.SQL.IdentifierUniqueness](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/SQL/IdentifierUniqueness.hs#L9)
  - line 32 of [Hasura.Backends.Postgres.SQL.IdentifierUniqueness](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/SQL/IdentifierUniqueness.hs#L32)

# Postgres identifier length limitations

Postgres truncates identifiers to a maximum of 63 characters by default (see
https://www.postgresql.org/docs/12/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS).

