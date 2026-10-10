# AGENTS.md (metadata/data-source)

Nx project `metadata-data-source`, import path `@hasura/metadata/data-source`.
`src/index.ts` re-exports `./driver`, `./hooks`, `./types`, `./utils` (flat
barrel, no default export).

## Purpose

The console's database-abstraction layer for the table/data browser and
schema-editing UIs. It defines a single `Database` interface (introspection,
query, modify, config, check, utilities) and provides one implementation per
supported database driver (postgres, alloydb, citus, cockroach, mssql,
bigquery, and a generic `gdc` implementation for all other Data
Connector-based drivers), so the rest of the console can call driver-agnostic
functions without branching on database type. It also ships a large set of
React Query hooks (`hooks/`) that wrap these driver methods for use in
components, plus shared utils/types for tables, capabilities, and
relationships.

## Modules

- **src/index.ts** — barrel: re-exports `driver`, `hooks`, `types`, `utils`.
- **driver/** — the core abstraction layer.
  - `driver/types/database.ts` — the `Database` type: `introspection`
    (getVersion, getDriverInfo, getDatabaseConfiguration,
    getDriverCapabilities, getTrackableTables, getDatabaseHierarchy,
    getTableColumns/getTableColumnInfos, getFKRelationships,
    getTablesListAsTree, getSupportedOperators,
    getTrackableFunctions/getTrackableObjects, getDatabaseSchemas,
    getSupportedDataTypes/getSupportedScalars, getStoredProcedures,
    getFunctionDefinition, getTableComment), `query.getTableRows`, `modify`
    (createDatabaseSchema, createTable/modifyTable/dropTable,
    changeTableName, alterTableComment/alterViewComment,
    dropFunction/alterFunctionComment, insertRows/updateRows/deleteRows,
    createForeignKey/alterForeignKey/dropForeignKey), `config`
    (getDefaultQueryRoot, getSupportedQueryTypes, getViolationActions,
    getFrequentlyUsedColumns), `check` (isFeatureSupported,
    isSchemaModification, isTable), `utilities.parseCreateSchemaSQL`. Also
    exports `DatasourceSqlQueries` type and re-exports `NotImplementedError`
    (defined in `@hasura/shared/types`).
  - `driver/types/{introspection,relationship,table,types}.ts` — prop/result
    types for all `Database` methods (`GetTableRowsProps`,
    `IntrospectedTable`, `TableColumn`, `DriverInfo`,
    `SupportedFeaturesType` — the legacy per-feature capability flag object
    — `AllowedTableRelationships` union, `InsertRowProps`/`UpdateRowProps`/
    `DeleteRowProps`).
  - `driver/dataSources.ts` — the driver registry/dispatcher: `drivers` map
    (`postgres`, `bigquery`, `citus`, `mssql`, `gdc`, `cockroach`, `alloy`),
    `getDatabaseMethods(driver)` (maps `'pg'`→postgres, native drivers→their
    impl, anything else→`gdc` fallback), `getDatabaseKind`,
    `getAllSourceKinds` (merges server-reported source kinds with a
    hardcoded AlloyDB entry, since AlloyDB isn't reported by the server —
    it's just Postgres under the hood), `getDataSourceSqlQueries`,
    `getConnectDatabaseFormSchema` (builds a zod schema for the "connect
    database" form from a driver's OpenAPI config schema, via
    `transformSchemaToZodObject` from `@hasura/shared/ui`).
  - `driver/index.ts` — `DataSource(args)` factory: given `{ endpoints,
fetchJson, driver }`, returns `{ introspectTables }`. Re-exports
    `types/database`, `utils`, `common/utils`, `common/sqlUtils`, `guards`,
    `postgres`, `types`, `dataSources`.
  - `driver/guards.ts` — relationship type guards: `isRemoteDBRelationship`,
    `isRemoteSchemaRelationship`, `isLegacyRemoteSchemaRelationship`,
    `isManualObjectRelationship`, `isSameTableObjectRelationship`,
    `isLocalTableObjectRelationship`, `isManualArrayRelationship`,
    `isLocalTableArrayRelationship`, `isLegacyFkConstraint`. Distinguishes
    relationship shapes purely by structural narrowing (presence of
    `to_source`/`hasura_fields`/`manual_configuration`/
    `foreign_key_constraint_on` keys) — order/specificity of checks matters
    since shapes overlap.
  - `driver/utils.ts` — shared driver-level utils.
  - **Per-driver folders** (`postgres/`, `bigquery/`, `citus/`,
    `cockroach/`, `mssql/`, `gdc/`, `alloydb/`), each generally following:
    `index.ts` (barrel + assembles the `Database` object), `database.ts`,
    `types.ts`, `introspection/`, `query/getTableRows.ts`, `modify/`,
    `sqlQueries.ts`, `utils.ts`.
    - **postgres/** — the most complete/reference implementation (also
      used for `cockroach`, `citus`, `alloy` via aliasing). Has extra
      introspection (getFunctionDefinition, getTableComment,
      getDatabaseConfiguration) and a full `modify/` set.
    - **citus/cockroach** — smaller, Postgres-derived variants; reuse much
      of `common/`.
    - **bigquery** — different shape (no FK relationships, dataset/project
      hierarchy); `introspection/getTablesListAsTree.tsx` (JSX, builds
      `TreeDataNode` tree nodes) and `modify/defaultQueryRoot.ts`.
    - **mssql** — has schema create/delete, `getIsTableView`,
      `getStoredProcedures`, `getVersion`, but no full row-modify support
      (no insert/update/delete row builders, unlike postgres).
    - **gdc** (Generic Data Connector) — the fallback implementation used
      for every driver not in the native list (Snowflake, Athena, etc.).
      Its `introspection/` calls generic capability/schema-introspection
      endpoints rather than hardcoded per-DB SQL.
    - **alloydb/** — thin wrapper; mostly delegates to postgres (AlloyDB is
      Postgres-compatible), exports `AlloyDbTable` type.
  - **driver/common/** — shared logic reused across native SQL drivers.
    - `capabilities.ts` — `isFeatureSupported(feature, supportedFeatures)`
      (safe lodash-`get`-based lookup), plus `postgresCapabilities`/
      `postgresSupportedFeatures` constants (the canonical/most-permissive
      feature-flag set; other drivers' capability objects are typically
      partial subsets).
    - `getAllSourceKinds/` — fetches server-reported source kinds.
    - `getTableName/` — `getTableName` (also re-exported at
      `driver/index.ts`).
    - `graphqlUtil.ts` — GraphQL name/type helpers.
    - `modify/{deleteRows,insertRows,updateRows}.ts` — generic REST/SQL
      row-mutation builders shared by SQL-ish drivers.
    - `sqlQueries.ts`, `sqlUtils.ts`, `utils.tsx`, `validation.ts` — shared
      SQL string builders and validators.
  - **driver/types/** — cross-driver prop/result types.
- **hooks/** — React Query (`@tanstack/react-query`) hooks layered on top
  of `driver/`; every hook resolves `getDatabaseMethods(source.kind)` and
  calls the matching `Database` method, using `useAppContext()`/
  `useAuthFetchJson()` from `@hasura/shared/context`/`@hasura/shared/hooks`.
  - `hooks/introspection/` — `useDriverCapabilities`,
    `useAllDriverCapabilities`, `useAvailableDrivers`,
    `useDatabaseConfiguration`, `useDatabaseVersion`, `useStoredProcedures`,
    `useSupportedDataTypes`, `useSupportedScalars`, `useTableColumns`,
    `useTreeData`.
  - `hooks/table/` — `useTableComment`, `useTableEnums`,
    `useTableForeignKeys`/`useTablesForeignKeys`, `useTableInfos`,
    `useTrackableTables`, `useTrackedAndUntrackedTables`, `useIsTableView`.
  - `hooks/function/` — `useFunctionDefinition`, `useTrackableFunctions`,
    `useTrackedAndUntrackedFunctions`.
  - `hooks/rows/` — `useRows` (core browse-rows query; exports
    `getBrowseRowsQueryKey`, `getRowsColumns`, `DEFAULT_STALE_TIME = 0` —
    see Gotchas), `useInsertRows`, `useEditRows`, `useDeleteRows`,
    `useExportRows/` (CSV/JSON export helpers).
  - `hooks/modify/` — mutation hooks: `useCreateTable`, `useDropTable`,
    `useChangeTableName`, `useAlterTableComment`, `useAlterViewComment`,
    `useCreateForeignKey`, `useAlterForeignKey`, `useDropForeignKey`,
    `useCreateDatabaseSchema`, `useDeleteDatabaseSchema`, `useDropFunction`,
    `useAlterFunctionComment`. All throw `NotImplementedError` when the
    resolved driver lacks the corresponding optional `modify.*` method.
  - `hooks/relationship/useCreateTableRelationships/` — relationship-
    creation hook with its own `types.ts`/`typeGuards.ts`/`utils.ts`.
- **types/** — `types/table.ts`, `types/queryKey.ts` (centralized React
  Query key builders: `getTrackableFunctionsQueryKey`,
  `getTrackableTablesQueryKey`, `getTableEnumsQueryKey`,
  `getTableCommentQueryKey`, `getTableForeignKeysQueryKey`,
  `getTablesForeignKeysQueryKey`, `getTableColumnInfosQueryKey`).
- **utils/** — `utils/capabilities.ts` (`supportsSchemaLessTables`),
  `utils/table/` (`foreignKey.ts`, `selectors.ts`, `table.ts`) — generic
  table/FK helpers not tied to a specific driver.

## Gotchas

- Driver dispatch is centralized in `getDatabaseMethods` (`driver/dataSources.ts`):
  special-cases `'pg'` → postgres, checks `isNativeDriver` (from
  `@hasura/metadata/helpers`) for other native kinds, and falls back to
  `gdc` for anything else. Adding a new native driver requires updating
  both the `NativeDriver` type upstream and the `drivers` record here.
- Not all drivers implement all `Database` capabilities — most
  `introspection`/`modify` methods are optional. Callers (mostly
  `hooks/modify/*`) must check for existence and throw
  `NotImplementedError` if missing; mssql, for example, has no row
  insert/update/delete builders, unlike postgres/citus/cockroach.
- AlloyDB is synthesized, not server-reported: `getAllSourceKinds` manually
  appends an `alloy` entry because the server doesn't return it as a
  distinct source kind; its SQL queries alias directly to
  `postgresSqlQueries`/postgres `database.ts`.
- `SupportedFeaturesType` (legacy) vs `Capabilities` (dc-api-types) are two
  parallel capability systems — native drivers populate both
  `check.isFeatureSupported` and `introspection.getDriverCapabilities`.
  Don't confuse the two when adding feature gates.
- Relationship type guards in `driver/guards.ts` rely on overlapping
  structural shapes — adding a new relationship shape needs care not to be
  misclassified by an earlier, looser guard.
- `DataSource()` factory (`driver/index.ts`) has a commented-out/dead
  `getIsTableView` method referencing a `Feature.NotImplemented` sentinel
  that doesn't appear to exist elsewhere — treat as stale/incomplete code.
- Some introspection files (e.g. `getTablesListAsTree.tsx`) build
  `TreeDataNode` tree UI (the `Tree` type from `@hasura/shared/ui`) directly inside what's nominally a data-layer
  package — this library is not purely headless/logic-only.
- `hooks/rows/useRows`'s `DEFAULT_STALE_TIME` is intentionally `0` (not a
  more efficient value) due to a known interaction with
  `queryClient.invalidateQuery` not propagating to leaf-level hooks — see
  the inline TODO before "fixing" this.
- The package's own `README.md` names the wrong Nx target (`nx test
data-source`); the actual project name is `metadata-data-source`.
- `getConnectDatabaseFormSchema` dynamically builds a zod schema from a
  driver's OpenAPI `configSchema`/`otherSchemas` via
  `transformSchemaToZodObject` — form validation for "connect database" is
  generated, not hand-written, so schema bugs manifest as form-validation
  bugs, not TS errors.

## Testing

Vitest (`vite.config.mts`, `environment: 'jsdom'`, `globals: true`). 11 test
files, mostly under `__tests__/` subfolders (`driver/bigquery/introspection/`,
`driver/common/` ×4, `driver/postgres/`,
`hooks/relationship/useCreateTableRelationships/`,
`hooks/rows/useExportRows/`, `hooks/rows/useRows/`, `utils/table/`). Run via
`nx test metadata-data-source`.
