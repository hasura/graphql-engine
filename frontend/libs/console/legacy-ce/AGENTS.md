# AGENTS.md (console/legacy-ce)

Nx project `console-legacy-ce`, import path `@hasura/console-legacy-ce`. Tags
`scope:console`, `type:feature`, `meta:legacy`. Workspace-wide commands,
the lib map and the Storybook basics are in `frontend/AGENTS.md`. This file
covers only what's specific to this lib.

## Purpose

Almost all of the console UI (CE). `apps/console-ce` is a thin shell around
it. `libs/console/legacy-ee` (and `apps/console-ee`) import its route
builders and components and add EE features on top. All source is TS/TSX
(no `.js`). Redux was removed on this branch, along with `react-router-dom`,
antd, LaunchDarkly and the old `services/`/redux-era Data modules.

## Public API (`src/index.ts`)

- App entry: `ConsoleCeApp` (`src/lib/client.tsx`), `App`, `AppProvider`, `Login`.
- Route builders, used by CE and EE route trees: `makeDataRouter`,
  `getActionsRouter`, `getEventRoutes`, `getRemoteSchemaRoutes`,
  `getAllowListRoutes`, plus `RestEndpoints` and `Settings` re-exports.
- Feature entry points: ApiExplorer, VoyagerView, SchemaRegistry, EETrial
  (`WithEELiteAccess`, SSO/multi-secret pages), Prometheus, OpenTelemetry,
  QueryResponseCaching, FeatureFlags, ControlPlane, telemetry, auth
  (`lib/shared/auth`).
- Legacy globals: `globals`, `endpoints`/`Endpoints`, `tableScss`,
  `DragFoldTable`.

EE depends on these exports (33 files in `legacy-ee`/`apps` import this lib).
Don't rename or remove an export without grepping `libs/console/legacy-ee`
and `apps/` first.

## Layout (`src/lib`)

- `client.tsx`: provider stack `ReactQueryProvider` (from `@hasura/metadata/api`)
  → `AppProvider` → `AppTheme` → `BrowserRouter basename={globals.urlPrefix}`
  → `Router`.
- `Router.tsx`: the CE route tree. `navigation.ts`: `RouteChangeListener`
  (scrolls to the hash on navigation).
- `features/<Name>/` (30 folders, run `ls src/lib/features` to see them):
  where new code goes. The largest are `Data` (AddTable, ModifyTable,
  ManageTable, BrowseRows, LogicalModels, ...), `Permissions`, `Eventing`
  (AdhocEvents/CronTriggers/EventTriggers), `RemoteSchema` and `Actions`.
  `features/components/` holds shared non-atomic components.
- `components/`: legacy shell only. `App`, `Main` (header, `UserMenu`,
  layout), `Login`, `Error` (`ErrorBoundary`, `PageNotFound`), `Common/`
  (`Layout`, `Onboarding`, `Table`, `TableCommon`, `Icons`).
- `shared/auth` (`AuthProvider`, admin-secret/no-auth hooks), `shared/utils`
  (GraphQL SDL helpers), `hooks/` (control-plane/cloud API clients),
  `telemetry/`, `theme/tailwind.css`.
- `Globals.ts`/`Endpoints.ts`: module-level values read from `window.__env`.
  These are legacy. In components, use `useAppContext()` (`envVars`,
  `endpoints`) from `@hasura/shared/context` instead.

## State & data fetching

- No Redux. Global state lives in React Context: `AppContext` (built in
  `components/App/AppProvider.tsx` from `window.__env` and `useServerVersion`)
  and `AuthContext` (`shared/auth/AuthProvider.tsx`).
- Zustand appears in exactly 3 stores: `components/App/store.ts`
  (`useGlobalLoadingStore`), `telemetry/store.ts` (`useTelemetryStore`) and
  `features/IsFeatureEnabled/store.tsx`. Use it only for state local to a
  feature.
- Server state uses `@tanstack/react-query` v5. Prefer the hooks in
  `@hasura/metadata/api`: `useMetadata` (used most), `useMetadataMigration`
  for writes, `useInvalidateMetadata`/`useReloadMetadata`, `runMetadataQuery`,
  `useServerConfig`. Source-level operations go through
  `@hasura/metadata/data-source`. Feature-specific hooks sit in
  `features/<Name>/hooks/`.
- Feature-scoped contexts exist, for example
  `features/Data/context/{CurrentTableContext,DataSourceContext}.ts`.
- Notifications: use `hasuraToast` (103 files). The `legacyNotifications`
  helpers (`showErrorNotification` and friends, 27 files) are legacy, so
  don't add new call sites.

## Forms

Use `useConsoleForm({ schema, options: { defaultValues } })` plus `SimpleForm`
and the field components from `@hasura/shared/ui`, with a `zod` schema. Type
values with `z.infer<typeof schema>`. Reference implementation:
`features/Data/ModifyTable/components/TriggerForm.tsx` and its test. Avoid raw
`useForm` and `@hookform/resolvers` (1 remaining site).

## Routing (react-router v8)

- Import everything from `react-router` (`Route`, `Routes`, `Navigate`,
  `Link`, `useNavigate`, `useParams`, `MemoryRouter`). `react-router-dom` no
  longer exists.
- Routes are JSX `<Route>` trees, not a data router. Each feature exports a
  builder that returns `<Route>` fragments (`features/Data/routes.tsx`,
  `features/Eventing/routes.tsx`, `features/Actions/routes`, RemoteSchema,
  RestEndpoints, AllowLists), and `Router.tsx` composes them under `<Main>`.
  When you add a route that EE also needs, add it in the builder, not in
  `Router.tsx`.
- `features/Data/Legacy/*Redirect.tsx` redirect old v2 URLs. Keep them.
- Router regressions are covered by `shared/reactRouterV8.regression.test.tsx`
  and `shared/reactRouterV8.realRoutes.regression.test.tsx`.

## Styling & dark mode

- Tailwind v4 (`@tailwindcss/postcss`) plus Radix Themes via
  `@hasura/shared/ui`. `src/lib/theme/tailwind.css` only imports `tailwindcss`
  and defines `@custom-variant dark (&:where(.dark, .dark *))`.
- The root `tailwind.config.js` is **not loaded**: `@config` is commented out
  in `apps/console-*/src/css/tailwind.css` and most of its theme is commented
  out. Custom tokens such as `legacybg` don't exist, so use stock Tailwind
  classes or Radix CSS variables (`var(--gray-11)` and so on).
- Dark mode: `ThemeProvider`/`AppTheme` in `libs/shared/ui/src/theme` puts
  `light`/`dark` on `<html>` and persists it to localStorage
  (`hasura-console:appearance`). Read it with `useAppearance()`. Prefer Radix
  tokens, which switch automatically, over `dark:` utilities (only 3 files
  use `dark:`).
- 12 `*.module.scss` files remain (`Main.module.scss`, `TableCommon`, and a
  few others). Don't add new SCSS. `clsx` is the classname helper. Import
  lodash per function (`lodash/get`), because ESLint bans the bare `lodash`
  import.

## Testing

- Vitest (`vite.config.mts`) with jsdom, globals, `TZ=UTC`. Setup file is
  `src/setupTests.ts`: it adds jest-dom matchers, mocks `lottie-react` and
  stubs `window.__env`. `src/lib/setupTests.ts` is an unused leftover.
  CSS, image and font imports resolve to `src/emptyModuleStub.ts`.
- There are 132 test files (126 `*.test.ts(x)`, 6 `*.spec.ts`), co-located or
  under `__tests__/`.
- Wrap hooks and components with `testWrapper` or `testRenderWithClient` from
  `@hasura/shared/testing` (QueryClient with `retry: false`, `MemoryRouter`
  and `AppContext`). Data hooks need router context (`useAuthFetchJson` uses
  `useNavigate`).
- Network: `setupServer` from `msw/node` (22 files) reusing the feature's
  `mocks/handlers.mock.ts`.
- jsdom has no `ResizeObserver`. Tests that render Radix dialogs or popovers
  stub it locally (see `TriggerForm.test.tsx`).

## Storybook

- `nx run console-legacy-ce:storybook` runs on port 4400 and also serves
  `libs/shared/ui` stories (see the `stories` globs in `.storybook/main.ts`).
  `@storybook/addon-mcp` is enabled.
- There are 171 `*.stories.tsx` files and 53 use `play`. The global
  decorators in `.storybook/preview.tsx` are described in `frontend/AGENTS.md`.
  `ToastsHub` comes in through `AppTheme`. There's also an `appearance`
  toolbar (light/dark) and a `parameters.mockdate` decorator.
- MSW 3: `preview.tsx` defines a custom `setupMsw` with `onUnhandledFrame`.
  Handlers live in `features/<Name>/**/mocks/handlers.mock.ts` (some older
  folders use `__mocks__/`), usually as `parameters: { msw: handlers() }`.
- Await every `play` interaction. `@typescript-eslint/no-floating-promises`
  is an error in stories.

## Gotchas

- Targets are inferred, so `project.json` isn't the full list. Real targets:
  `lint`, `lint-fix`, `test`, `storybook`, `build-storybook`,
  `test-storybook`, `static-storybook`, `chromatic`. There's no `build` (the
  lib is bundled by `apps/console-*`) and no `typecheck`.
- Build-time globals (`CONSOLE_ASSET_VERSION`, `__DEV__`, ...) are defined in
  `vite.config.mts` and declared in `eslint.config.mjs`. The app build defines
  them in `tools/webpack/console-webpack-tweaks-plugin.js`. A new global needs
  all three.
- Only 2 class components remain, both error boundaries. Don't add more.

## Checks before finishing

Run from `frontend/`:

```sh
npx nx run console-legacy-ce:lint
npx nx run console-legacy-ce:test   # add -- <path filter> to narrow
npx tsc --noEmit -p libs/console/legacy-ce/tsconfig.lib.json
npx tsc --noEmit -p libs/console/legacy-ce/tsconfig.spec.json
```

`lint` is clean (0 errors), so keep it that way. `tsc` isn't clean yet. As of this writing it reports about 413 errors (lib)
and 247 (spec), mostly in test fixtures/mocks and in `libs/open-api-to-graphql`.
So don't expect zero errors. Instead, make sure files you touched add no new
errors (grep the output for your paths). Run `build-storybook` if you
changed stories or `.storybook/`.
