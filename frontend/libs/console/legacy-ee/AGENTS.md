# AGENTS.md (console/legacy-ee)

Nx project `console-legacy-ee`, import path `@hasura/console-legacy-ee`.
Tags `scope:console`, `type:feature`, `meta:legacy`. Only consumer:
`apps/console-ee/src/main.tsx` (renders `<Main />`).

## Purpose

The EE / Pro / Cloud console shell: its own app root, route tree, header/nav,
enterprise auth (admin secret, PAT, SSO, Hasura Cloud SSO), privilege-based
route guards, and the Monitoring ("pro") metrics UI. Nearly every page it
routes to is imported from `@hasura/console-legacy-ce`; EE adds the shell
around them.

## Public API (`src/index.ts`)

```ts
export { Main } from './lib/client';
```

That is the whole surface. Don't export more unless `apps/console-ee` needs it.

## Layout (`src/lib/`, 199 files: 79 tsx, 50 ts, 59 svg, 7 scss, 2 css, 2 png)

- `client.tsx` — `Main`: `ReactQueryProvider` > CE `AppProvider` >
  `AppTheme` > `BrowserRouter basename={globals.urlPrefix}` > CE
  `RouteChangeListener` + `Router`.
- `Router.tsx` — the full EE `<Routes>` tree (react-router v8).
- `Globals.ts` — spreads CE `globals` and adds EE env (`metricsApiUrl`,
  `hasuraOAuthUrl`, `isPATSet`, `ssoIdentityProviders`, `projectId`, `pro`...).
- `shared/auth/` — `AuthProvider`, `useEnterpriseAuth`, `useSsoAuth`,
  `useNavigateAuth`, `privileges.ts` (`checkAccess`), `types.ts`
  (`EnterpriseAuthState`, `Privilege`), `context.ts` (`useAuthContext`).
- `components/Main/` — EE header/nav (`Main.tsx`, `HeaderNavItem`, `metadataStatusRedirect.ts`).
- `components/Login/` — `Login` picks `LoginEELite` (`consoleType ===
'pro-lite'`) or `LoginProCloud`; plus `AdminSecretLogin`, `SSOLogin*`.
- `components/OAuthCallback/`, `components/AccessDenied/`.
- `components/RouteGuard/` — `PrivilegesRouteGuard`, `DataRouteGuard`.
- `components/Services/Metrics/` — Monitoring tab under `/pro`
  (`MetricsRouter.tsx`): Overview, Operations, Usage, Error, StatsPanel
  filters, FlameGraph, chart.js via `react-chartjs-2` (`registerCharts.ts`).
- `apollo.config.ts` — `makeApolloClient` (HTTP + `graphql-ws`); used only by
  Metrics, which mounts its own `ApolloProvider` against the metrics server.
- `hooks/useProjectInfo.ts` — React Query fetch of Cloud project privileges
  and entitlements via CE `ControlPlane` (returns `null` unless `cloud`).
- `helpers/`, `utils/` — small local utilities (`redirectUtils.ts` holds
  `restrictedPathsMetadata` used by the guards).

## How it extends CE

- No registration or injection hooks. EE composes CE: it imports pages,
  route factories (`makeDataRouter`, `getActionsRouter`, `getEventRoutes`,
  `getRemoteSchemaRoutes`, `getRestRoutes`, `getAllowListRoutes`), `App`,
  `globals`, auth-storage helpers (`saveConsoleAuthState`...),
  `ControlPlane`, `WithEELiteAccess`, `EnterpriseNavbarButton` from
  `@hasura/console-legacy-ce` (15 files import it) and wires them into its own
  `Router.tsx`.
- EE-lite license/trial UI (`EETrial`, `WithEELiteAccess`) lives in CE, not here.
- Auth plugs in through `AuthContext` from `@hasura/shared/context`:
  `AuthProvider` blocks render until `initialize()` finishes, then provides an
  `EnterpriseAuthService`. Read it with `shared/auth/context.ts`'s
  `useAuthContext` (typed), not the untyped shared one.
- Dependency direction: CE must never import EE. No tag rule in
  `eslint.config.mjs` forbids it (both are `scope:console` +
  `type:feature`); it would surface as an Nx circular-dependency error from
  `@nx/enforce-module-boundaries`. Put shared code in CE or `libs/shared/*`.

## State and routing

- State = React Context (`AppContext`,
  `AuthContext`) + React Query (`useProjectInfo`, metadata hooks) + Apollo
  for Metrics only.
- `react-router` v8 only (`react-router-dom` is gone). Guards are layout
  routes rendering `<Outlet />` after an effect check (they replace v3
  `onEnter`); they redirect with `navigate(..., { replace: true })`.
- Feature gating by env: `isMonitoringTabSupportedEnvironment`, `consoleType`
  (`pro`/`pro-lite`/`cloud`) and `consoleMode` (`server`/`cli`).

## Testing

Vitest (`vitest.config.mts`, jsdom, globals on, setup
`tools/test-setup/setupTests.ts`). Only 3 test files / 15 tests:
`Main/EEUserMenu.test.tsx`, `Main/metadataStatusRedirect.test.ts`,
`Services/Metrics/Common/FlameGraph.test.tsx`. Test extracted pieces, not the
whole authenticated shell (see `EEUserMenu`'s comment). Anything importing
the `@hasura/shared/ui` barrel needs `vi.mock('lottie-react', ...)` (lottie
needs canvas; jsdom has none). Copy the pattern in `EEUserMenu.test.tsx`.

## Gotchas

- No `storybook` target, and no Storybook config includes this lib's
  stories.
- `tsconfig.spec.json` doesn't load jest-dom types, so `tsc` reports
  `toBeInTheDocument`/`toHaveAttribute` errors in the two `.test.tsx` files.
  Vitest still passes. These errors were already there; don't chase them.
- `tsc -p tsconfig.lib.json` follows imports into other libs and type-checks
  them with this lib's `strict` settings (~140 errors in
  `libs/open-api-to-graphql`). Filter the output to `libs/console/legacy-ee`.
- `eslint.config.mjs` turns off most `react-hooks/*` rules here (legacy code);
  guards rely on effects with partial deps on purpose.
- `.babelrc` remains (`@nx/react/babel`), but tests run on Vite.

## Checks before finishing (from `frontend/`)

```sh
npx nx test console-legacy-ee          # 3 files, 15 tests pass
npx nx lint console-legacy-ee          # clean
npx tsc --noEmit -p libs/console/legacy-ee/tsconfig.lib.json | grep 'libs/console/legacy-ee'
                                       # only the known Modal.stories.tsx error
npx nx run console-ee:build            # when touching routing/providers/exports
```
