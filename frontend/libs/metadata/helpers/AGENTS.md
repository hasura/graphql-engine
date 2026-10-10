# AGENTS.md (metadata/helpers)

Nx project `metadata-helpers`, import path `@hasura/metadata/helpers`.
`src/index.ts` re-exports `openTelemetry`, `source`, `table`,
`inconsistentMetadata`, `header`, `function`, `metadata` flatly, plus
`export type * from './selector/types'`, and a namespaced `MetadataSelectors`
(from `./selector`, not flattened — access as `MetadataSelectors.xxx`).

## Purpose

Framework-agnostic helper library for working with Hasura's GraphQL Engine
metadata document (the JSON tree describing sources, tables, functions,
permissions, relationships, etc.), consumed alongside `@hasura/shared/types`.
It provides type guards/predicates for metadata shapes, normalization/equality
helpers for the several table-reference formats each database kind uses
(schema/name, dataset/name, GDC array form, bare string), and a library of
composable "selector" functions for plucking data out of a `Metadata` object
(sources, tables, roles, event triggers, relationships, native
queries/logical models). It also includes small parsers for OpenTelemetry
config validation and header (env-var vs literal value) transformation used
by the console UI.

## Modules

- **index.ts** — barrel; re-exports `openTelemetry`, `source`, `table`,
  `inconsistentMetadata`, `header`, `function`, `metadata` flatly;
  re-exports `selector/types` as types only; exposes `selector/*` under
  namespace `MetadataSelectors` (not flattened).
- **inconsistentMetadata.ts** — `findInconsistentRemoteSchema` (matches on
  `type === 'remote_schema'` and `name === "remote_schema ${name}"` — a
  literal string-prefix match, not a plain name comparison),
  `findInconsistentSource` (matches `type === 'source'` and `sourceName ===
obj.definition`).
- **metadata.ts** — `isMetadataEmpty(metadataObject)`: true unless there's
  at least one remote schema, one action, or one source with a non-empty
  `tables` array.
- **openTelemetry.ts** — `parseOpenTelemetry`,
  `parseUnexistingEnvVarSchemaError`, `parseHasuraEnvVarsNotAllowedError` —
  thin `.safeParse()` wrappers around zod schemas from
  `@hasura/shared/types`; never throw, caller must check `.success`.
  Non-strict discriminated-union parsing means unexpected extra server
  fields are silently accepted/stripped, not rejected.
- **function/function.ts** (barrel via `function/index.ts`) —
  `adaptFunction(qualifiedFunction: TableFunction): QualifiedFunction`
  normalizes the 3 accepted shapes (1-element array, 2-element array, bare
  string, or object passthrough) into `{schema, name}`, defaulting schema to
  `'public'` — comment notes this assumption only holds because
  Postgres-family is the only native DB supporting functions. `search`
  filters by `"schema / name"` case-insensitive substring match.
  `functionDisplayName` builds a display string, optionally prefixed with
  data source name. `areFunctionsEqual`/`areFunctionsEqualCoalesce` are
  aliases of `table/predicate`'s `areTablesEqual`/`areTablesEqualCoalesce`.
- **source/index.ts** — driver/source-kind type guards: `isPostgresFlavour`
  (true for postgres/citus/cockroach/alloy), `getDriverPrefix` (returns
  `'pg'` for postgres-flavour, else the driver name itself),
  `isPostgresSource`, `isMssqlSource`, `isBigQuerySource`, `isCitusSource`,
  `isCockroachSource` (all `Source` discriminant narrowers on `.kind`),
  `isNativeDriver`/`isKnownEnterpriseSourceKind` (membership checks against
  constants from `@hasura/shared/types`).
- **table/index.ts** — barrel re-exporting `table`, `predicate`,
  `permission`.
- **table/table.ts** — `isSchemaTable`, `isDatasetTable`, `isGDCTable`
  (alias `isGDCFunction`) type guards for the 3 table-reference shapes.
  `extractTableInfo(table): QualifiedTable | null` normalizes any shape
  into `{schema, name}`; returns `null` for unrecognized shapes.
  `getTableDisplayName` — best-effort generic display-name formatter with
  many branches (array, string/number, schema/dataset object,
  `table_name`/`table_schema` object, generic `name` object, arbitrary
  object — falls back to sorting all string-valued keys and joining, or
  `JSON.stringify` as last resort); returns literal `'Empty Object'` for
  null/undefined/empty-array input. `getTableLabel` builds `"source /
schema / name"`-style labels per table kind, returns `''` for
  unrecognized shape.
- **table/predicate.ts** — `areTablesEqual`: structural/shallow equality
  (handles null/undefined, arrays element-wise, then plain-object shallow
  key/value equality) — does NOT normalize differing table-reference
  shapes first. `areTablesEqualCoalesce`: normalizes both sides via
  `extractTableInfo` first, then compares `name`/`schema` — the
  "smart"/cross-shape-safe comparison; prefer this one when shapes may
  differ.
- **table/permission.ts** — `isDataQueryType` guard against
  `DATA_QUERY_TYPES`. `TablePermissionWithType` discriminated union.
  `flattenTablePermissions` flattens a table's 4 permission arrays into one
  tagged array. `findPermissionByQueryAndRole` looks up a single
  permission by query type + role.
- **selector/index.ts** — barrel re-exporting `metadata`, `nativeQuery`,
  `table` (exposed as the `MetadataSelectors` namespace from the root
  barrel).
- **selector/types.ts** — `NativeQueryWithSource = NativeQuery & {source:
Source}`, `LogicalModelWithSource = LogicalModel & {source: Source}`.
- **selector/metadata.ts** — the largest file; low-level `Metadata`-object
  query/selector functions, largely curried `(...args) => (m: Metadata) =>
result` for composing with a `useMetadata(selector)` hook (from
  `@hasura/metadata/api`). Key exports: `findMetadataSource`,
  `findMetadataTable`, `findMetadataFunction` (lowest-level utilities
  reused by other selectors); `getSources`, `findSource`,
  `findNativeQuery`, `findLogicalModel`, `getTables`, `selectFunctions`;
  `findTable`, `findFunction`, `resourceVersion`,
  `isCollectionInAllowlist`, `getLocalDBObjectRelationships`/
  `getLocalDBArrayRelationships`, `getNewRolePermission`,
  `getForeignKeyRelationships`/`selectForeignKeyRelationships`;
  `selectRoles`/`getRoles` (aggregates every role referenced anywhere in
  metadata — actions, all 4 table-permission kinds, remote schemas,
  allowlist non-global scopes, API limits' `per_role`, logical-model
  select permissions — dedupes via `Set`); `findRemoteSchema`,
  `getOperationsFromQueryCollection`, `getRemoteDatabaseRelationships`/
  `getRemoteDatabaseRelationshipsFromSource`, `findMetadataSourceSchemaNames`,
  `getAllRemoteSchemaRelationships`, `findRemoteSchemaRelationship`,
  `selectRawEventTriggers`, `selectEventTriggers` (attaches `{name, schema,
source}` table info to each), `selectEventTriggerByName`,
  `selectEventsTriggersByTable`, `selectManualEventsTriggers`,
  `getSupportsForeignKeys(source)` (returns `false` only for bigquery — an
  "assume true unless known-false" heuristic).
- **selector/table.ts** — `findMetadataTableCoarse`: looser table lookup
  than `findMetadataTable`, matches by extracted `schema`+`name` on a raw
  list; returns `undefined` if `tables`, `schema`, or `name` is falsy.
- **selector/nativeQuery.ts** — `extractModelsAndQueriesFromMetadata`:
  walks every source and collects `logical_models`/`native_queries`,
  tagging each with its parent `source`, into `{models, queries}`.

## Gotchas

- `selector/metadata.ts` imports `adaptFunction`, `areTablesEqual`,
  `extractTableInfo` from `@hasura/metadata/helpers` (the package's own
  public import path) rather than relative paths, while everything else
  uses relative imports — an unusual "self-import via public API" pattern
  worth knowing about before refactoring the barrel or renaming the alias.
- `areTablesEqual` vs `areTablesEqualCoalesce` are not interchangeable:
  `areTablesEqual` does raw structural/shallow equality on whatever shape
  it's given — a `SchemaTable` object and a functionally-equivalent GDC
  array/tuple for the same table compare unequal. `areTablesEqualCoalesce`
  normalizes both operands via `extractTableInfo` first and should be used
  whenever the two sides might come from different DB-kind representations.
  `function/function.ts` aliases both onto the table versions — same
  distinction applies to functions.
- `getTableDisplayName`'s fallback chain is order-dependent and lossy: for
  arbitrary objects lacking recognized shape keys, it sorts keys
  alphabetically and joins string values, or falls back to
  `JSON.stringify` if any value isn't a string — adding a new table-shape
  variant without updating this function silently degrades to a fallback
  rather than erroring.
- `findInconsistentRemoteSchema` matches on a literal string prefix
  (`"remote_schema ${remoteSchemaName}"`, not just `remoteSchemaName`) —
  passing a name that already includes the prefix silently fails to match.
- `getSupportsForeignKeys` is an allow-list-by-exclusion: currently only
  returns `false` for `bigquery`; adding a new FK-unsupported source kind
  requires remembering to update this function directly.
- `isGDCFunction` is a bare alias of `isGDCTable` — no function-specific
  shape checking; relies on functions and tables sharing the same tuple-array
  wire representation for GDC sources.
- `adaptFunction`/`extractTableInfo` default to schema `'public'`/`''` for
  bare-string or single-element-array input — a Postgres-family assumption
  that would need revisiting if a non-Postgres native DB gained function
  support.
- `getRoles`'s dedup happens only at the very end via `Set`, so the
  aggregated list is unordered by source but stable in category order
  (actions → table permissions → remote schemas → allowlist → API limits →
  logical-model select permissions).
- `openTelemetry.ts` parsers never throw — callers must check
  `.success`/`.error`, and non-strict discriminated-union parsing means
  unknown extra fields from a newer server are silently tolerated.
- `transformHeaderConfigs`/`parseHeaderConfigs` are asymmetric on empty
  values: `transformHeaderConfigs` filters out falsy-`value` headers before
  transforming; `parseHeaderConfigs` always maps every input header
  (defaulting to `''`), with no equivalent filter.
- `package.json`'s `name`/`main`/`type` fields describe the built CJS
  output, not meaningful for in-repo TS consumption (which goes through the
  `tsconfig.base.json` path alias).

## Testing

Vitest (`vitest.config.mts`, `environment: 'node'`, `globals: true`). 3 test
files (`openTelemetry.test.ts`, `table/predicate.test.ts`,
`table/table.test.ts`), colocated with source. Run via `nx test
metadata-helpers` (the README's `nx test helpers` is stale — don't use it).
