# AGENTS.md (shared/utils)

Nx project `shared-utils`, import path `@hasura/shared/utils`. `src/index.ts`
re-exports everything (flat barrel, no default export).

## Purpose

A grab-bag of small, dependency-light, framework-agnostic helper functions
shared across the console frontend apps — string/data/date manipulation,
browser/storage wrappers, error normalization, env/console-mode detection,
and GraphQL query/mutation-string generation for the table/data browser UI.

## Modules

- **zod.ts** — `reqString(name)`: zod schema for a trimmed, required string.
- **string.ts** — `matchAll`, `replaceAll` (polyfill-safe),
  `generateRandomString`, `hashString` (SubtleCrypto digest),
  `getReadableNumber` (locale-formatted).
- **env.ts** — `parseConsoleType` (throws on unknown type),
  `getEnvVarAsString`, `getEnvVarAsBoolean`.
- **proConsole.ts** — Console-mode/tier gating: `isProConsole`,
  `isMonitoringTabSupportedEnvironment`,
  `isCachingEnabled`, `isOpenTelemetrySupported`, `isCloudConsole`,
  `getProjectId`, `getTenantId`.
- **error.ts** — `isConsoleError`, `getErrorMessage`: deeply-nested heuristic
  extractor for error messages from `HttpError`/GraphQL/Postgres-shaped
  error objects (many fallback branches, order matters).
- **localStorage.ts** — `setLSItem`/`getLSItem`/`removeLSItem`,
  `setLSItemWithExpiry`/`getItemWithExpiry` (TTL-based), `getParsedLSItem`,
  `clearGraphiqlLS`. Hasura Cloud monkey-patches localStorage per-project;
  shared keys must be added to `GLOBAL_LS_KEYS` in the `lux` repo.
- **version.ts** — `checkValidServerVersion`, `getFeaturesCompatibility`
  (semver-based feature flags), `versionGT`, `checkStableVersion` (uses
  `semver`).
- **protocol.ts** — `getWebsocketProtocol`, `replaceURLHttpToWs` (http→ws URL
  conversion, falls back to `window.location`).
- **url.ts** — `getPathRoot`, `stripTrailingSlash`, `isValidURL`,
  `isURLTemplated`, `isValidTemplateLiteral` (`{{...}}` template detection).
- **data.ts** — Type guards/predicates (`isNotNull`, `isNumberString`,
  `isJsonString`, `isArray`, `isObject`, `isTypedObject`, etc.), `isEmpty`,
  `isEqual` (deep, JSON.stringify-based for arrays), array helpers
  (`deleteArrayElementAtIndex` mutates via `splice`!), `getAllJsonPaths`
  (recursive JSON path extractor).
- **export.ts** — `convertRowsToCSV`/`downloadObjectAsCsvFile`,
  `convertRowsToJSON`/`downloadObjectAsJsonFile` — browser-only (uses
  `document`).
- **browser.ts** — `getConfirmation`: wraps `window.confirm`/`prompt` for
  destructive-action confirmation flows.
- **date.ts** — `convertDateTimeToLocale` (date-fns), `safeParseInt`/
  `safeParseFloat` (note: `Number.parseInt`/`parseFloat` never throw, so the
  try/catch fallback is effectively dead code).
- **file.ts** — `uploadFile` (DOM-based file picker + FileReader),
  `encodeFileContent`, `getFileExtensionFromFilename`.
- **request.ts** — `request`/`requestJson`: `fetch` wrapper normalizing
  non-2xx responses and network failures into `HttpError` (from
  `@hasura/shared/types`).
- **sessionStorage.ts** — Typed `sessionStore` (`getItem`/`setItem`/
  `removeItem`) keyed by a hardcoded `SessionStorageKeys` schema (extend this
  type to add keys).
- **graphql/** — Largest submodule: generates raw GraphQL query/mutation
  strings for table browsing (depends on `@hasura/shared/types`, `graphql`
  package). Subfolders: `query/` (`generateGraphQLSelectQuery` — async,
  builds `where`/`order_by`/`limit`/`offset`; `getQueryRoot`), `mutation/`
  (`generateGraphQLInsertMutation`, `generateGraphQLDeleteMutation`,
  `generateGraphQLDeleteByPrimaryKeyMutation`, `getMutationRoot`), `common/`
  (`getTypeName`, `formatGraphQL`, gql name-validation regex/error-notif
  constants). Root-name resolvers all follow the same priority: custom root
  field name > custom table name > default, then apply operation suffix and
  source-level prefix/suffix.

## Gotchas

- Several modules (`browser.ts`, `export.ts`, `file.ts`, `localStorage.ts`)
  assume a DOM/`window` global — not safe for SSR/Node use.
- `getErrorMessage` has deep, order-sensitive fallback branching for
  Postgres/GraphQL error shapes — check existing branches before adding new
  error formats.
- No default export anywhere; always import named exports.

## Testing

Vitest (`vitest.config.mts`), Node environment, globals enabled. 12 test
files (`*.test.ts`/`*.spec.ts`) colocated with source. Run via
`nx test shared-utils`.
