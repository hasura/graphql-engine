# Step 3: Translating GraphQL queries to SQLite SQL

Now we need to start translating queries into SQL as understood by SQLite.

We'll put this in [Hasura.Backends.SQLite.Translate](../server/src-lib/Hasura/Backends/SQLite/Translate.hs).

For the translation procedure we take a simple approach of mapping AST nodes in
the IR 1-to-1 with clauses in the resulting SQL, inlining every occurring variable.

Next up is [Step 4: Executing queries](4-executing-queries.md)
