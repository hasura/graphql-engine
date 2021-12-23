# Limit over join optimization

## Metadata

```
---
authors: Gil Mizrahi <gil@hasura.io>
discussion:
   https://github.com/hasura/graphql-engine-mono/pull/2239
state: draft
---
```

## Description

Optimize GraphQL queries containing a relationships and a limit by limiting the amount of returned results
before joining the relationship.

### Problem

Currently when a user runs a complex query with relationships and is using the `limit` operator, we construct an SQL query for postgresql that looks somewhat like this:

```sql
SELECT *
  FROM <base-table> LEFT JOIN <other-table>
 LIMIT <limit>;
```

Since join is an expensive operation, it would be useful if we could limit the number of rows it needs to process before running the join.

In SQL, trying to push limits down into each side of the join is *not* a semantic perserving operation.  This is because the relationship between the two sides is unspecified, and could be one-to-one, many-to-one, or many-to-many.

For example, in a database of users and streaming providers, a user could be subscribed to multiple providers, and streaming providers provide services to multiple users. trying to get all users and their providers, limit by 10, is different than:

1. Limiting to 10 users and match their providers.  Because there can be more than 1 provider for each user - we might get more than 10 results
2. Limiting to 10 providers and match their users.  Because there can be more than 1 user for each provider - we might get more than 10 results
3. Limit to 10 for both users and providers.  Because some users might not use the selected providers, so we might get less than 10 results

For this reason, postgresql will not apply this optimization when *it is* valid, because it cannot distinguish the cases.

Fortunately, in GraphQL we do specify either have a one-to-one relationship, which means that we can limit one side and get the same result, or we have a one-to-many relationship where we aggregate the results, so we can limit the side of the "one", this side is always the root table in the query, or the "base" table.

### Why is it important?

It can improve the performance of queries by orders of magnitude ([as described by a customer](https://github.com/hasura/graphql-engine/issues/5745#issuecomment-899081795)).
Was requested by customers ([graphql-engine/#5745](https://github.com/hasura/graphql-engine/issues/5745)) which consider this feature a must-have.

## How

Implement this optimization ourselves by pushing the LIMIT into the base table.  This has a few caveats:

1. Both LIMITs and OFFSETs should be pushed to the base table
2. When ORDER BY is also involved, the order by should also be *duplicated* in the base table, so we can limit the results *after*  sorting, and *also* sort at the final results generation (in the `json_agg` function), otherwise the results order is unspecified.
3. When *DISTINCT* is involved, it should also be pushed into the base table - distinct acts as a filter and may reduce the amount of rows, so it should happens before limiting the results.

Because of (2) and (3) this optimization is only valid when the columns referred from any
DISTINCT or ORDER BY are from the base table. If other columns exist, this optimization is not valid.

### Success

We can verify the feature works by writing tests inspecting the generated SQL.

### Future Work / Out of Scope

Work on this features has been implemented in [#2239](https://github.com/hasura/graphql-engine-mono/pull/2239) by changing the way we translate `RQL` ASTs to postresql ASTs. This optimization might be better to express as an SQL to SQL transformation.
In order to refactor this code, we'd need to first [document the Postgres.Translate.Select](https://github.com/hasura/graphql-engine-mono/issues/2391) module. After that we could refactor this optimization to a straightforward translation of RQL to SQL and then an SQL transformation.
