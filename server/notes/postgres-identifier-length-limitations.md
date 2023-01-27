This note is in [Hasura.Backends.Postgres.SQL.RenameIdentifiers](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/SQL/RenameIdentifiers.hs#L35).

# Postgres identifier length limitations

Postgres truncates identifiers to a maximum of 63 characters by default (see
https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS).

