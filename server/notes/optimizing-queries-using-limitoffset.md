This note is in [Hasura.Backends.Postgres.Translate.Select](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/Translate/Select.hs#L761).
It is referenced at:
  - line 67 of [Hasura.Backends.Postgres.Translate.Types](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/Translate/Types.hs#L67)

# Optimizing queries using limit/offset

Refer to the issue https://github.com/hasura/graphql-engine/issues/5745

Before this change, limit/offset/distinct_on is applied at outer selection
node along with order by clause. This greatly reduces query performance if
our base selection table contains many rows and relationships are selected
which joins remote tables. We need to optimize application of limit wrt to
order by input.

If "Order by" is not present:
  Apply limit/offset/distinct on at the base table selection
Else if "Order by" contains only columns:
  Apply limit/offset/distinct_on at the base table selection along with order by
Otherwise:
  Apply limit/offset/distinct_on at the node selection along with order by

