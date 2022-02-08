This note is in [Hasura.Backends.Postgres.SQL.IdentifierUniqueness](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/SQL/IdentifierUniqueness.hs#L18).
It is referenced at:
  - line 6 of [Hasura.Backends.Postgres.SQL.IdentifierUniqueness](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/SQL/IdentifierUniqueness.hs#L6)
  - line 25 of [Hasura.Backends.Postgres.SQL.IdentifierUniqueness](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/SQL/IdentifierUniqueness.hs#L25)

# Postgres identifier length limitations

Postgres truncates identifiers to a maximum of 63 characters by default (see
https://www.postgresql.org/docs/12/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS).

