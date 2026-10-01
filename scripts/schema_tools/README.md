# schema_tools

`gen_schema.py` generates a **compatible** (not identical) Postgres schema from
a Hasura metadata export, so that metadata can be applied to a fresh database
without needing the customer's real DB schema dump.

This exists because it's common to get a customer's Hasura metadata for
investigation without also getting their database, and `hasura metadata
apply` (or tracking things via the console/API) needs a real Postgres schema
underneath it to succeed -- table/column existence gets validated, and for
some relationship types, real foreign key constraints get validated too.

The generated schema is deliberately **not** a reconstruction of the real
customer schema. It only needs to be structurally close enough that metadata
application succeeds and basic GraphQL queries (including relationship
traversal) execute without Postgres type errors. Row data, real column types,
defaults, check constraints, indexes, etc. are all out of scope.

## Usage

```sh
python3 gen_schema.py <metadata.json> <out_schema.sql> <out_report.txt> [seed]
```

- `metadata.json` -- a Hasura metadata export (the combined JSON form, e.g.
  what you get by concatenating `hasura metadata export` into one file).
- `out_schema.sql` -- the generated DDL: `CREATE TABLE`, real `FOREIGN KEY`
  constraints where Hasura requires them, and best-effort function stubs.
- `out_report.txt` -- every place the script had to guess, ordered by table.
  Read this before trusting the output for anything beyond "metadata apply
  doesn't error."
- `seed` -- optional integer (default `0`) seeding the random column-type
  draw (see Types below). Same metadata + same seed always produces the same
  schema; change the seed to get a different (still internally consistent)
  type assignment.

Load the SQL into an empty Postgres database, point a fresh Hasura instance
at it, then `replace_metadata` (or `hasura metadata apply`) with your
metadata. To POST the metadata directly instead of using `hasura metadata
apply`, wrap it yourself first:

```sh
python3 -c "
import json
m = json.load(open('metadata.json'))
json.dump({'type': 'replace_metadata', 'args': m}, open('replace_metadata.json', 'w'))
"
curl -s -X POST http://localhost:8080/v1/metadata \
  -H "X-Hasura-Admin-Secret: <secret>" -H "Content-Type: application/json" \
  -d @replace_metadata.json
```

No worked example is checked into this directory -- see Verification below
for why, and how to reproduce one against your own metadata file.

## How it works

Hasura's metadata.json does **not** record column types, primary keys, or
constraints -- that's all live-introspected from the DB and never
round-tripped into metadata. What it does leak, which is enough to work
with:

1. **Column names**: the union of columns referenced across every role's
   `select_permissions` / `insert_permissions` / `update_permissions` for a
   table. If a column is never exposed to any role, it's invisible to this
   method and won't appear in the generated table at all.
2. **Foreign keys**, two different ways:
   - `foreign_key_constraint_on` relationships. **These require a real
     Postgres FK constraint to exist**, or `metadata apply` reports them as
     inconsistent (confirmed empirically -- see Verification below). The
     format differs by relationship kind:
     - *object* relationships: `using.foreign_key_constraint_on` is a bare
       string -- the FK column, which lives on **this** table. The target
       table isn't named explicitly, so it's inferred from Hasura's default
       relationship-naming convention (object relationship name == target
       table name).
     - *array* relationships: `using.foreign_key_constraint_on` is
       `{"column": ..., "table": {...}}` -- naming the FK column and table
       **explicitly**, but that table is the far/many side, not this one.
   - `manual_configuration` relationships: `column_mapping` plus an explicit
     `remote_table`. These do *not* require a real FK constraint for
     `metadata apply` to succeed -- confirmed empirically -- just matching
     column names on both sides.
3. **Primary keys**: guessed from the `<table>_id` naming convention,
   reinforced by which column is most frequently the target of an inferred
   FK edge. This is a naming-convention heuristic, not a certainty -- see
   Limitations.
4. **Types**: not inferred at all. Instead, every column is assigned a type
   drawn at random (seeded, so reproducible) from a fixed weighted distribution
   meant to look like a typical OLTP schema (`TYPE_WEIGHTS` in the script:
   mostly `text`, `integer`, `bigint`, `uuid`, `timestamptz`, with smaller
   shares of `numeric`, `boolean`, `date`, `jsonb`). The one hard requirement
   is consistency, not correctness: every column reachable from another via a
   relationship edge (2) is unioned into the same group via union-find, and a
   group gets exactly one random type for every column in it -- otherwise a
   join between them would need an explicit cast and Postgres would reject the
   relationship's generated SQL at query time.
5. **Custom functions**: metadata only records the function's name and
   `exposed_as` (`query`/`mutation`), never its arguments or return type. A
   no-op stub is emitted (`RETURNS SETOF <first tracked table>`, right
   volatility for query vs. mutation) purely so the function is trackable --
   it will not do anything resembling what the real function does.

## Verification

This was validated against a real customer's metadata export (37 tables, 149
relationships) by actually standing up a throwaway Postgres +
`hasura/graphql-engine` in Docker, loading the generated schema, and calling
`replace_metadata` with the metadata (remote schemas stripped -- see
Limitations). Result: `is_consistent: true`, zero inconsistent objects, and a
GraphQL query traversing an array relationship executed successfully.

That customer's metadata file, and everything generated from it (schema SQL,
warning report, wrapped `replace_metadata` payload), is intentionally **not**
checked into this directory -- it's a real customer's actual table/column
names and full role/permission structure, which shouldn't end up committed
just because the customer name itself was scrubbed from it. Only the script
and this README are checked in.

To reproduce the verification yourself against any metadata file you have
locally:

```sh
python3 gen_schema.py metadata.json schema.sql report.txt

# fresh throwaway postgres + hasura
docker network create metatest-net
docker run -d --name metatest-pg --network metatest-net \
  -e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=postgres postgres:15
docker run -d --name metatest-he --network metatest-net -p 18080:8080 \
  -e HASURA_GRAPHQL_DATABASE_URL="postgres://postgres:postgres@metatest-pg:5432/postgres" \
  -e HASURA_GRAPHQL_ADMIN_SECRET=test \
  hasura/graphql-engine:v2.44.0

# load the schema
docker cp schema.sql metatest-pg:/schema.sql
docker exec metatest-pg psql -U postgres -d postgres -f /schema.sql

# strip remote_schemas / remote_relationships / opentelemetry first if present
# (see Limitations), then wrap and apply the metadata
python3 -c "
import json
m = json.load(open('metadata.json'))
m['version'] = 3
json.dump({'type': 'replace_metadata', 'args': m}, open('replace_metadata.json', 'w'))
"
curl -s -X POST http://localhost:18080/v1/metadata \
  -H "X-Hasura-Admin-Secret: test" -H "Content-Type: application/json" \
  -d @replace_metadata.json

# cleanup
docker rm -f metatest-pg metatest-he && docker network rm metatest-net
```

`report.txt` is the warning report produced alongside the SQL -- worth
reading to see the specific tables where PK/type guesses were made or
skipped.

## Limitations / future work

- **Remote schemas and remote relationships are out of scope.** The script
  doesn't touch them at all -- it only ever emits Postgres DDL. The metadata
  used for Verification had 3 remote schemas and 2 remote relationships,
  which had to be stripped out by hand before `replace_metadata` would
  succeed, since applying them requires live, reachable GraphQL endpoints
  that have nothing to do with the Postgres schema. A real fix would be
  generating trivial mock GraphQL servers (schema-only, matching each
  `remote_schema`'s expected fields) alongside the SQL, or teaching the
  script to just drop these sections automatically with a warning instead of
  requiring a manual strip.
- **PK inference is a naming convention, not a fact.** It works well when a
  table's PK column is literally `<table_name>_id` (~30/37 tables in the
  source metadata), but fails silently (no PK, not a wrong one usually) for
  tables using a different abbreviation convention for their PK column (e.g.
  a heavily-abbreviated table name whose PK doesn't literally contain that
  abbreviation) or where the real PK column is never exposed to any role via
  permissions and so is invisible to this method entirely (a handful of
  view-like and lookup tables in the source metadata). No PK just means
  Hasura won't generate a `*_by_pk` root field -- it doesn't block `metadata
  apply`.
- **Types are random, not derived, by design.** There is no source of ground
  truth for real column types anywhere in metadata.json, and a naming-suffix
  heuristic (tried initially) turned out to be a fit to one customer's
  convention rather than a general rule. Randomizing (while keeping
  relationship-joined columns consistent with each other) avoids overfitting
  to any one file's style, but it will still be wrong for any column whose
  real type actually matters -- e.g. a numeric column used in a GraphQL
  `_sum`/`_avg` aggregate, or a column compared against a typed literal in a
  permission filter. This file's permission filters/checks were all empty
  (`{}`), which sidestepped that failure mode entirely -- a metadata file
  with real boolean-expression filters would be a meaningfully harder case
  and hasn't been tested here.
- **Custom function stubs are placeholders, not guesses at real behavior.**
  The one custom mutation function in the source metadata got a zero-arg
  no-op stub just so it's trackable; if a metadata file has custom functions
  whose *names* don't hint at a table to return, even that guess gets
  harder.
- **Single-column FK/relationship keys only.** Composite (multi-column)
  `column_mapping`s and composite `foreign_key_constraint_on` are not
  specially handled beyond looping each pair independently, which is fine
  for type-unification purposes but hasn't been exercised against a metadata
  file that actually uses composite keys.
- **Only tested against one particular metadata file.**
