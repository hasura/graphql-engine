# AGENTS.md (metadata/api)

Nx project `metadata-api`, import path `@hasura/metadata/api`. `src/index.ts`
re-exports `./hooks`, `./utils`, `./types`, `./api`, and
`./mocks/metadata.mock` (flat barrel, no default export).

## Purpose

Provides the React Query (`@tanstack/react-query`) hooks and low-level
`fetch`-wrapper functions the Hasura console frontend uses to talk to a
GraphQL Engine server's Metadata API, Schema/GraphQL endpoint, and
`run_sql`/migrate endpoints. It centers on `useMetadata` (fetches/caches the
whole exported metadata doc) and `useMetadataMigration` (generic mutation
wrapper for any metadata query type, with built-in cache invalidation,
out-of-date/conflict handling, and toast notifications), on top of which
dozens of narrower hooks are built for tracking/untracking tables and
functions, managing permissions, relationships, remote schemas, REST
endpoints, query collections, sources, and running SQL. It also exposes plain
(non-hook) API functions (`exportMetadata`, `runSQL`, `runGraphQL`,
`runIntrospectionQuery`, etc.) for use outside React, plus small utilities for
error toasting and inconsistent-metadata parsing.

## Modules

- **api/** — Framework-agnostic network calls, each taking `{ url,
fetchJson, ... }` (`NetworkArgs` from `types.ts`, wrapping
  `@hasura/shared/utils`'s `FetchJson`).
  - `catalog.ts` — `fetchCatalogState`/`setCatalogState`: get/set console UI
    state (notifications-read state, onboarding flag) via
    `get_catalog_state`/`set_catalog_state` metadata queries.
  - `exportMetadata.ts` — `exportMetadata`: POSTs `{type: 'export_metadata',
version: 2}`, returns full `Metadata` (`@hasura/shared/types`).
  - `getMigrationStatus.ts` — `getMigrationStatus(url)`: GETs CLI-server
    migrate-settings endpoint, maps `migration_mode === 'true'` to
    `'healthy'`/`'unhealthy'`.
  - `notification.ts` — `fetchConsoleNotifications`: runs a raw GraphQL
    query against `console_notifications` with scope/time filtering; throws
    first `GraphQLError` if `errors` present.
  - `runGraphQL.ts` — `runGraphQL<T>`: POSTs a GraphQL body; throws the
    first `result.data.errors[0]` as `GraphQLError` (response status is 200
    even on GraphQL errors).
  - `runIntrospectionQuery.ts` — `runIntrospectionQuery`: calls `runGraphQL`
    with `getIntrospectionQuery({descriptions:true,
schemaDescription:true, typeDepth:7})`.
  - `runMetadataQuery.ts` — `runMetadataQuery<ResponseType>` + types
    `TMigrationSingleQuery`/`TMigrationBulkQuery`/`TMigrationQuery`: generic
    POST for any metadata query type. This is the foundational type used by
    nearly every mutation hook in the package.
  - `runSQL.ts` — `getRunSqlQuery`/`runSQL`/`runSQLBulk`; `getRunSqlType`
    maps driver → `{prefix}_run_sql` where `postgres`/`alloy` both map to
    prefix `pg`.
  - `runSQLMigrate.ts` — `runSQLMigrate` (CLI-server `/apis/migrate`
    endpoint) and `getDownQueryComments` (auto-generates a commented-out
    "down" migration SQL block when none is supplied).
  - `version.ts` — `loadServerVersion`, `loadLatestServerVersion` (checks
    against `updateCheck` endpoint, tagged with `agent=console`).
  - `index.ts` — barrel re-exporting all of the above.
- **types.ts** — `NetworkArgs<T>` (`{url, fetchJson}`), `SchemaResponse`,
  `TableRelationship`.
- **hooks/constants.ts** — `DEFAULT_STALE_TIME` (5 min), query keys
  (`METADATA_QUERY_KEY`, `INCONSISTENT_METADATA_QUERY_KEY`,
  `MIGRATION_STATUS_QUERY_KEY`), `MAX_ALLOWED_MIGRATION_LENGTH` (255 − 14
  for unix-epoch prefix), `defaultQueryOptions`.
- **hooks/metadata/** — Core metadata read/write hooks.
  - `useMetadata.ts` — `useMetadata(selector?, options)`: React Query
    wrapper around `exportMetadata`; keyed on `METADATA_QUERY_KEY`;
    `refetchOnWindowFocus: true`; default `staleTime` 5 min.
  - `useMetadataHelpers.ts` — `useMetadataHelpers()` returns
    `{fetchMetadata, fetchSource}`, imperative fetch/refetch helpers used
    inside mutation callbacks; throws if source not found.
  - `useMetadataMigration/` — the central mutation primitive.
    - `useMetadataMigration.ts` — `useMetadataMigration<ResponseType,
ArgsType>(options)`: wraps `runMetadataQuery` in `useMutation`;
      supports `errorTransform`; on success calls
      `usePostMetadataMigration()` (cache invalidation + CLI sync) then the
      user's `onSuccess`; on error calls `toastMetadataOutOfDateError`
      (detects HTTP `conflict` code and prompts user to refetch) then the
      user's `onError`. Nearly every other hook in the package composes
      this.
    - `usePostMetadataMigration.ts` — in `consoleMode === 'cli'`, triggers
      an extra query to re-export metadata from the CLI server's local
      filesystem; always invalidates metadata cache.
  - `useMetadataVersion.ts` — `useMetadataVersion()` = `useMetadata(d =>
d.resource_version)`.
  - `useInconsistentMetadata.ts` — `useInconsistentMetadata`,
    `useFetchInconsistentMetadata` (imperative),
    `useInvalidateInconsistentMetadata`, `inconsistentSourcesSelector`.
  - `useInvalidateMetadata.ts` — `useInvalidateMetadata()`/standalone
    `invalidateMetadata(queryClient)`: invalidates both metadata and
    inconsistent-metadata query keys together (always paired).
  - `useReloadMetadata.tsx` — `useReloadMetadata()`: computes which
    sources/remote schemas are inconsistent and issues a `reload_metadata`
    query scoped to just those, unless caller forces
    `shouldReloadAllSources`/`shouldReloadRemoteSchemas`.
  - `useReplaceMetadata.ts` — `useReplaceMetadata()`: wraps
    `replace_metadata`; parses response `warnings[].code` into specific
    toast titles (`illegal-event-trigger-name`, `source-cleanup-failed`,
    else generic).
  - `useReplaceMetadataFromFile.ts` — JSON-parses a file string then calls
    `useReplaceMetadata`; toasts a parse error and calls `onError` without
    ever calling `replaceMetadata` if parsing fails.
  - `useResetMetadata.ts` — `useResetMetadata()`: `clear_metadata`; always
    invalidates in a `.finally()` even on error.
  - `useDropInconsistentMetadata.ts` — thin wrapper issuing
    `drop_inconsistent_metadata`.
- **hooks/graphql/** — `useGraphQLMutation` (generic named GraphQL mutation
  runner), `useIntrospectSchema` (builds a `GraphQLSchema` via
  `buildClientSchema`, cached under `INTROSPECT_SCHEMA_QUERY_KEY`).
- **hooks/query/** — `useRunSQLQuery`/`useRunSQLCommand` (select vs.
  command variants of `runSQL`), `useRunSQLBulk`.
- **hooks/table/** — `useTrackTable`, `useTrackTables` (bulk; strips
  `logical_model` out of `configuration`), `useUntrackTable`,
  `useUntrackTables`. All derive the metadata query-type prefix from the
  source driver via `getDriverPrefix` (`@hasura/metadata/helpers`) and
  require a `useMetadataHelpers().fetchSource` round-trip first.
- **hooks/function/** — `useTrackFunction`/`useTrackFunctions`,
  `useUntrackFunctions`, `useSetFunctionCustomization`,
  `useRetrackFunction` (drop+re-track as one `bulk` migration, merging old
  and new `configuration`; matches old function via `areFunctionsEqual`).
- **hooks/permission/** — `useCreateTablePermission` (exports only the
  query builder `getCreatePermissionQuery`, no hook),
  `useDropTablePermission`/`useDropTablePermissionMultipleRoles`,
  `useUpdateTablePermission` (strips `limit` from select-permission
  definition when `limitEnabled` is false), `useCreateFunctionPermission`/
  `useDropFunctionPermission`, `useAddInheritedRole`/`useUpdateInheritedRole`
  (drop+add as bulk)/`useDropInheritedRole`.
- **hooks/relationship/** — `useCreateRemoteRelationship`,
  `useUpdateRemoteRelationship`, `useDeleteRemoteRelationship`,
  `useRenameRelationship`, `useDropRelationship`, `useDropRelationships`
  (bulk_atomic; auto-detects whether each relationship is "remote" to pick
  the right drop query type), and `useSuggestedRelationships/`:
  - `useSuggestedRelationships` — queries `{driver}_suggest_relationships`,
    cross-references already-tracked FK relationships from metadata (via
    `selectors/selectors.ts`'s `getTrackedSuggestedRelationships`) and
    computes GraphQL-safe constraint names (`utils.ts`'s
    `addConstraintName`).
  - `mock.ts` — Chinook-dataset fixtures for tests/stories only.
- **hooks/remoteSchema/** — `useAddRemoteSchema`, `useUpdateRemoteSchema`,
  `useRemoveRemoteSchema`, `useIntrospectRemoteSchema`,
  `useMetadataRemoteSchemas.ts` (exports `useListRemoteSchemas`),
  `useReloadRemoteSchema`, `useDropRemoteSchemaPermissions`/
  `useDropRemoteSchemaPermissionMultipleRoles` (confirms via
  `getConfirmation` before dropping), `useSaveRemoteSchemaPermission`
  (drop+recreate if a permission for that role already exists).
- **hooks/rest/** — REST endpoint CRUD built on top of query collections:
  `useAddRestEndpoint` (creates `allowed-queries` collection first if
  missing), `useCreateRestEndpoints`, `useDeleteRestEndpoints`,
  `useDropRestEndpoint`, `useEditRestEndpoint`, `useRestEndpoint` (pure
  metadata lookup, no network), and `useRestEndpointDefinitions/` —
  generates REST endpoint + GraphQL query definitions from introspection
  using `microfiber`; derives per-operation HTTP path/method per
  `EndpointType` (`READ`/`READ_ALL`/`CREATE`/`UPDATE`/`DELETE`).
- **hooks/source/** — `useDropSource` (`{driver}_drop_source`, always
  `cascade: true`).
- **hooks/queryCollections/** — `useCreateQueryCollection` (also exports
  `createAllowedQueriesIfNeeded`, reused by REST-endpoint hooks),
  `useAddOperationsToQueryCollection`, `useEditOperationInQueryCollection`,
  `useMoveOperationsToQueryCollection` (also re-points any REST endpoint
  referencing a moved query), `useRemoveOperationsFromQueryCollection`
  (also drops any REST endpoint built on a removed query),
  `useDeleteQueryCollections`, `useRenameQueryCollection`.
- **hooks/migration/useMigrationStatus.ts** — `useMigrationStatus()`: only
  meaningful in `consoleMode === 'cli'` (returns `'off'` immediately in
  server mode with `staleTime: Infinity`); `updateMigrationModeStatus()`
  PUTs the toggled state to the CLI server.
- **hooks/server/** — `useServerConfig` (`staleTime: Infinity`, since
  server config doesn't change at runtime), `useServerVersion` (imperative
  `initServerVersion()`; computes `featuresCompatibility` via
  `getFeaturesCompatibility` from `@hasura/shared/utils`, fetches
  latest-available version for update-nag banners).
- **utils/** — `error.tsx`: `toastMetadataOutOfDateError` (detects
  `HttpError` with `data.code === 'conflict'`, shows a toast with a "Fetch
  metadata" action button) and `handleMigrationStatusError` (CLI-mode
  `alert()` messaging). `hyperlinkErrorMessageLink.tsx`:
  `createTextWithLinks` (regex-based auto-linkifier for warning/error
  message text). `inconsistentMetadata.ts`:
  `getRemoteSchemaNameFromInconsistentObjects` (documented "HACK" — see
  Gotchas) and `getSourceFromInconsistentObjects`.
- **mocks/metadata.mock.ts** — Test/story fixtures: `metadataTables`,
  `metadataSource` (sqlite), `metadataWithoutSources`,
  `metadataWithSourcesAndTables`, `metadataWithResourceVersion`
  (deliberately malformed for edge-case testing). Exported from the public
  barrel, so consumers outside this lib can import these mocks too.
- **stories/** — Storybook stories plus MSW mock handlers/data — not part
  of the public API surface (not re-exported from `src/index.ts`).

## Gotchas

- `useMetadataMigration` is the load-bearing abstraction almost everything
  else composes; its `onSuccess`/`onError` always run cache-invalidation
  and out-of-date-conflict toasting before/alongside the caller's own
  callbacks — don't bypass it by calling `runMetadataQuery` directly unless
  you also handle invalidation yourself.
- `useInvalidateMetadata`/`invalidateMetadata` always invalidate both
  `METADATA_QUERY_KEY` and `INCONSISTENT_METADATA_QUERY_KEY` together —
  there's no way to invalidate just one via this helper.
- Many hooks (`useTrackTable`, `useTrackFunction`, permission/relationship
  hooks, etc.) must resolve the data source's driver `kind` first via
  `useMetadataHelpers().fetchSource`/`fetchMetadata` before building the
  query type string via `getDriverPrefix` — order-sensitive: fetch source →
  derive prefix → build query → mutate.
- `getRunSqlType` maps both `postgres` and `alloy` drivers to the `pg`
  prefix — a non-obvious special case; other native drivers use their own
  name as prefix.
- `hooks/remoteSchema/useReloadRemoteSchema.ts` references a bare,
  undefined `name` identifier in its success/error toast messages instead
  of the `remoteSchemaName` parameter — likely throws/resolves to
  `undefined` at runtime when those branches execute; check before relying
  on that message text.
- `utils/inconsistentMetadata.ts`'s
  `getRemoteSchemaNameFromInconsistentObjects` is explicitly documented as
  a fragile "HACK": it extracts the remote schema name by taking the last
  whitespace-separated token of a free-text `name` field — breaks if that
  message format changes server-side.
- `useReplaceMetadata`'s warning-title mapping is order-sensitive and
  silently falls back to a generic title for any unrecognized code — easy
  to misattribute an unrelated warning code.
- Several hooks assume browser globals: `getConfirmation`/`alert()`-based
  flows (`useDropRemoteSchemaPermissions`, `handleMigrationStatusError`)
  are not SSR-safe.
- Heavy inter-dependency on other `@hasura/*` packages: `@hasura/shared/types`,
  `@hasura/metadata/helpers` (`getDriverPrefix`, `MetadataSelectors`,
  `areTablesEqual`, `areFunctionsEqual`, `getTableDisplayName`),
  `@hasura/shared/hooks` (`useAuthFetchJson`), `@hasura/shared/context` (`useAppContext`,
  `Endpoints`), `@hasura/shared/ui` (`hasuraToast`,
  `showErrorNotification`), `@hasura/shared/types` (`HttpError`,
  `EnvVars`, `ADMIN_SECRET_HEADER_KEY`), `@hasura/shared/utils`
  (`requestJson`, `getErrorMessage`, `getConfirmation`, `formatGraphQL`,
  `getFeaturesCompatibility`, `isNotNull`). This package cannot be used
  standalone without an `AppContext` provider supplying `endpoints`/`envVars`.
- Do not import from `@hasura/metadata/data-source` here: it already depends on
  this package, and the resulting project-graph cycle makes Nx drop the
  `^build` edges from `console-ce:build`/`console-ee:build` to the buildable
  libs, so webpack races their `dist/` rebuild. Shared pieces belong in
  `@hasura/shared/types` (e.g. `NotImplementedError`, `SchemaTable`).
- `useRestEndpointDefinitions` depends on GraphQL introspection succeeding
  and uses heuristics to locate query/mutation root types when custom
  root-field namespaces are configured; assumes `pk_columns`/`pkColumns`
  argument naming for update operations, throwing if neither is found.

## Testing

Vitest (`vite.config.mts`, `environment: 'jsdom'`, shared setup file
`../../../tools/test-setup/setupTests.ts`). 4 test files:
`hooks/metadata/useMetadataMigration/useMetadataMigrationServerMode.test.tsx`,
`hooks/metadata/useMetadataMigration/useMetataMigrationCLIMode.test.tsx`
(note the typo "Metata" in the filename),
`hooks/relationship/useSuggestedRelationships/selectors/selectors.test.ts`,
`hooks/rest/useRestEndpointDefinitions/utils.test.ts`. Run via `nx test
metadata-api`.

### Mocking metadata endpoints (`@hasura/shared/testing`)

Hook tests that hit `/v1/metadata` should use the shared MSW handlers instead
of hand-rolling a `setupServer` body:

```ts
import { setupServer } from 'msw/node';
import { handlers } from '@hasura/shared/testing';

// Each handlers() call owns an isolated in-memory metadata document, so
// mutations never leak across tests.
const server = setupServer(...handlers());

beforeAll(() => server.listen());
afterEach(() => server.resetHandlers(...handlers())); // fresh state per test
afterAll(() => server.close());
```

- The handlers match `POST */v1/metadata`, `GET */v1alpha1/config`, and
  `GET */apis/metadata` **by path** (wildcard origin), so they work for both
  server mode (`http://localhost/v1/metadata`) and CLI mode
  (`apiPort` origin for the CLI export). `resetHandlers(...handlers())` resets
  to a fresh document each test — plain `resetHandlers()` would keep the mutated
  state and cause cross-test flakiness.
- `export_metadata` returns the current document (with `resource_version`); a
  successful write returns `{ message: 'success' }` and bumps `resource_version`
  (so `useMetadataVersion` re-exports the new value after invalidation); a
  reducer error returns its `status` + error JSON. Action types without a domain
  handler (e.g. `pg_create_remote_relationship`) succeed and bump the version.
- Use `server.use(...handlers({ initialData, config, delay }))` or add extra
  `http.*` handlers to override per test.
- Render hooks with `testWrapper` / `testRenderWithClient` from
  `@hasura/shared/testing` (not `@hasura/shared/ui`); they provide the
  `QueryClientProvider`, `AppContext`, and a `MemoryRouter` (needed by
  `useAuthFetchJson`). `Button` and other components come from
  `@hasura/shared/ui`.

**Pre-existing (unrelated to the mocks):**
`hooks/rest/useRestEndpointDefinitions/utils.test.ts` fails to import
`./useRestEndpointDefinitions` (`getOperations` moved to `index.ts`); this
breakage predates the shared-handlers work.
