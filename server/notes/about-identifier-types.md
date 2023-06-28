This note is in [Hasura.Backends.Postgres.SQL.Types](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/SQL/Types.hs#L96).

# About identifier types

In order to better be able to reason about values representing SQL binders and
variables we are in the process of retiring the generic 'Identifier' type in
favor of the more specific types 'TableIdentifier' and 'ColumnIdentifier'.

Likewise, we distinguish binders of names from uses of names: The types
'TableAlias' and `ColumnAlias` are used to for binders, whereas
`TableIdentifier` and `ColumnIdentifier` represent usages or references of
previously bound names.

We want to ensure these are handled in an hygenic way:
\* 'TableAlias'es and 'ColumnAlias'es can be constructed freely, but
\* 'TableIdentifier' can only be constructed from a 'TableAlias' via
  'tableAliasToIdentifier', and
\* 'ColumnIdentifier's can only be constructed from a 'ColumnAlias', and
  potentially be qualified with a 'TableIdentifier'.


