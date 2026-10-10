# AGENTS.md (shared/hooks)

Nx project `shared-hooks`, import path `@hasura/shared/hooks`. A grab-bag of
small, reusable React hooks (lifecycle helpers, storage-backed state,
pagination state, click-outside detection, authenticated fetch) shared
across the frontend monorepo. No components, no styling — pure hooks.

## Public API (`src/index.ts`)

Barrel export, one file per hook (name matches file unless noted):

- `useDebouncedEffect(effect, delay, deps)` — runs `effect` after `delay` ms,
  debounced via `setTimeout`/`clearTimeout`; keeps latest `effect` in a ref
  so it isn't a dep.
- `useDocumentTitle(title)` — sets `document.title` on mount/change, restores
  previous title on unmount. Native replacement for `react-helmet`.
- `useAuthFetchJson()` (in `useFetch.ts`) — returns an async
  `<T>(url, config) => Promise<T>` fetcher. Pulls auth headers from
  `useAuthContext()`, merges into request headers; on header-fetch failure
  or a 401 `HttpError` response, redirects to `LOGIN_PATH` via
  `react-router`'s `useNavigate` and throws `UnauthorizedError`.
- `useIsFirstRender()` — returns a callback; `true` only on the first call,
  `false` thereafter (stateful, ref-based, not idempotent per render).
- `useIsUnmounted()` — returns a callback reporting mounted state, via a
  `'mounting'|'mounted'|'unmounted'` ref set in `useLayoutEffect`.
- `useLocalStorage(key, initialValue)` — `useState` tuple synced to
  `window.localStorage` (JSON serialized); SSR-safe (`typeof window`
  checks); swallows errors via `console.log`.
- `useOnClickOutside(refs, handler)` — listens for `mousedown`/`touchstart`
  on `document`; calls `handler` when the event target is outside all given
  refs.
- `usePagination(initialState?)` — local pagination state
  `{ sorts, limit, offset }`, default `limit: 10`, default sort
  `created_at asc nulls last`. Returns `{ paginationState, setPaginationState }`.
- `useSessionStoreState(key)` — `useState`-like tuple backed by
  `sessionStore` (from `@hasura/shared/utils`), typed via
  `SessionStorageKeys`/`PathInto`/`Choose` from `@hasura/shared/types`.
- `useUpdateEffect(effect, deps)` — like `useEffect` but skips the first
  render, using `useIsFirstRender` internally.

## Dependencies on other `@hasura/shared/*` packages

- `@hasura/shared/utils`: `requestJson` (useFetch), `sessionStore`
  (useSessionStoreState)
- `@hasura/shared/context`: `useAuthContext` (useFetch)
- `@hasura/shared/types`: `HttpError`, `LOGIN_PATH`, `UnauthorizedError`,
  `OrderBy` (usePagination), `Choose`/`PathInto`/`SessionStorageKeys`
  (useSessionStoreState)
- External: `react-router` (`useLocation`, `useNavigate`) in useFetch.

## Gotchas

- `useFetch.ts` does not export a `useFetch` hook — it exports
  `useAuthFetchJson`, which couples this "generic hooks" package to routing
  and auth context, unlike every other hook here.
- Several hooks silently swallow errors via `console.log` rather than
  surfacing them.

## Testing

Vitest via Nx (`nx test hooks`), config in `vitest.config.mts`. **No test
files currently exist** in `src/` — testing infra is set up but unused.
