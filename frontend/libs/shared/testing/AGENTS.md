# AGENTS.md (shared/testing)

Nx project `shared-testing`, import path `@hasura/shared/testing`.

## Purpose

Shared testing/Storybook toolkit for the console frontend: Storybook
decorators (React Query, React Hook Form, console-type/env switching), MSW
request handlers + fixture data for mocking the `/v1/metadata` and
`/v1alpha1/config` Hasura endpoints, and React Testing Library render
wrappers for hook/component tests.

## Public API (`src/index.ts`)

- `./storybook` — decorators + utils
- `./tests` — RTL test wrappers
- `./mocks` — MSW handlers + fixture data

### Storybook exports (`src/storybook`)

- `ReactQueryDecorator` (`decorators/react-query.tsx`) — wraps a story in a
  shared singleton `QueryClient` + `ReactQueryDevtools`; on HMR/story change
  calls `resetQueries()` (skipped on first mount via a module-level
  `timestamp` flag) so stale cached data isn't served across stories.
- `FormDecorator` (`decorators/react-hook-form.tsx`) — wraps story in
  `react-hook-form`'s `FormProvider`.
- `ConsoleTypeDecorator` + `storybook-globals`
  (`decorators/console-type/`) — renders a floating menu letting you toggle
  console type (oss/cloud/pro/etc.) and admin-secret state; writes to
  `window.__env` and Storybook globals, force-remounts children (via `key`)
  when settings change. Depends on `@hasura/shared/ui`
  (Button/ReactSelect/Switch) and `@hasura/shared/types` (`EnvVars`).
- `utils`: `dangerouslyDelay(ms)` (plain setTimeout promise; prefer
  `waitFor`/`findBy` in tests instead), `waitForRequest(...)` — **dead/broken**:
  built around msw-storybook-addon v2's global `getWorker()`, which v3
  removed; no live call sites remain (only commented-out usage).

### Test wrapper exports (`src/tests`)

- `testWrapper` / `testRenderWithClient(ui)` (`tests/decorator.tsx`) — wraps
  components in a fresh `QueryClientProvider` (retry disabled), a
  `MemoryRouter` (needed by hooks like `useAuthFetchJson` that call
  `useLocation`/`useNavigate`), plus `@hasura/shared/context`'s
  `AppContext.Provider` (using `defaultAppState`/`getEndpoints` against
  `window.__env` and `http://localhost`). `testRenderWithClient` returns RTL's
  `render()` result with a `rerender` that re-wraps.

### Mock exports (`src/mocks`)

Per-domain fixture data + reducer-style handler maps for metadata sections:
`allowList.ts`, `queryCollections.ts`, `openTelemetry.ts`, `source.ts`,
`rest.ts` — each exports `xInitialData` and `xHandlers` (keyed by metadata
action type, error shape `{ status, error: { path, error, code } }`).

`reducer.ts` — `metadataReducer(state, action)` folds an action onto the
in-memory `Metadata` document. It wires every per-domain `xHandlers` map plus
`export_metadata` together, unwraps `bulk` / `concurrent_bulk` / `bulk_atomic`
/ `bulk_keep_going` (each nested arg runs through the same reducer, first error
short-circuits), and treats **unknown** action types as a no-op success (so the
handlers stay usable for hooks whose type has no domain handler yet). Returns
either the new `Metadata` or `{ status, error }`; `isMetadataError()` narrows.
Also exports `MetadataReducer`, `MetadataAction`, `MetadataErrorResponse`,
`ResponseBodyMetadataTypeError`.

`metadata.ts` — `createDefaultInitialData()` merges the per-domain
`xInitialData`; `handlers(options)` returns MSW **v2** (`http` + `HttpResponse`)
handlers backed by an isolated clone of that document:

- `GET  {url|*}/v1alpha1/config` → `config`
- `GET  {url|*}/apis/metadata` → current document (CLI-mode export)
- `POST {url|*}/v1/metadata` → runs `metadataReducer`:
  - `export_metadata` → current document (no version bump)
  - successful write → commits new state, bumps `resource_version`, responds
    `{ message: 'success' }` (`METADATA_WRITE_SUCCESS`), **not** the whole doc
  - reducer error → its `status` + error JSON (no bump)

  Matches **by path** when `options.url` is omitted (wildcard origin, so the
  same handlers cover `http://localhost/v1/metadata` in tests and
  `http://localhost:8080/v1/metadata` in Storybook); pass `options.url` to pin
  an absolute origin. Other options: `delay`, `initialData`, `config`.

**Reuse in hook tests** (see `@hasura/metadata/api`): wrap `handlers()` with
`setupServer` from `msw/node` yourself — `setupServer(...handlers())`,
`afterEach(() => server.resetHandlers(...handlers()))` for fresh per-test state.
`msw/node` is intentionally **not** re-exported from this package's barrel so it
never lands in the Storybook (webpack) browser bundle.

## Gotchas

- `handlers()` is MSW **v2** and matches by path by default — don't assume the
  old `http://localhost:8080` origin or the v1 `(req, res, ctx)` signature.
- `resetHandlers()` with no args keeps the mutated in-memory document; pass
  `resetHandlers(...handlers())` to reset to a clean clone between tests.
- `waitForRequest` in storybook utils is dead code from an old msw-addon API
  — don't use it as a reference pattern.

## Dependencies on other `@hasura/shared/*` packages

`@hasura/shared/ui`, `@hasura/shared/types`, `@hasura/shared/context`,
`react-router`. Depends on `@hasura/shared/types` (**not**
`@hasura/metadata/api` — that would create a project cycle, since
`metadata/api` tests import this package).

## Testing

`@nx/vitest` (`nx test shared-testing` / `nx test testing`), config in
`vite.config.mts` (jsdom environment, globals on). `src/mocks/metadata.test.ts`
covers the reducer + handlers (export/write/version bump/error/state
isolation/CLI export).
