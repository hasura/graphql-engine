# AGENTS.md (frontend)

This covers the `frontend/` workspace only. See the repo-root `AGENTS.md` for
Haskell build/PR instructions that apply to the whole `graphql-engine-mono` repo.

## What this is

An Nx + Yarn monorepo for Hasura's console (the GraphQL Engine admin UI). It's a
pure frontend workspace (TypeScript/React) — no Haskell here. The console talks
to the Haskell `graphql-engine` server over HTTP (see `libs/shared/context/src/endpoints.ts`,
which builds URLs like `/v1/graphql`, `/v1/metadata`, `/v1/version` from env vars +
a base URL). Built console assets are packaged via `build-server-assets` for
embedding in the server binary / Hasura CLI.

Two editions: CE (community/OSS) and EE (enterprise). EE builds on top of CE.

## Toolchain

Node `>=24` (`.nvmrc`: v24.18.0), Yarn 4 (`packageManager: yarn@4.17.1`,
`nodeLinker: node-modules`). Only `yarn.lock` is allowed —
`yarn check-lock-files` (runs on `postinstall`) rejects `package-lock.json`,
`pnpm-lock.yaml` and local-registry references.

Key versions: Nx 23, TypeScript 6, React 19, react-router 8, webpack 5 (apps +
Storybook), Vite 8 / Vitest 5 (unit tests), Storybook 10, Cypress 15, MSW 3,
Tailwind 4, Radix UI Themes 3, React Query 5, Zustand 5, react-hook-form 7 +
zod 4, GraphiQL 5 (Monaco), ESLint 10 (flat config), Prettier 3.

Removed on this generation of the codebase — **don't reintroduce**: Redux /
`react-redux` / thunks, `react-router-dom` (import everything from
`react-router`), antd, Jest as a runner, `.eslintrc*` files.

## Layout and import paths

Folder → Nx project name → `@hasura/*` import path (from `tsconfig.base.json`).
Packages with their own `AGENTS.md` are marked 📄 — read it before working there.

- `apps/console-ce` 📄, `apps/console-ee` 📄 → `console-ce`, `console-ee` —
  the buildable webpack apps (thin shells).
- `apps/console-ce-e2e` 📄, `apps/console-ee-e2e` 📄 → `console-ce-e2e`,
  `console-ee-e2e` — Cypress e2e.
- `apps/nx/internal-plugin-e2e` 📄 → `nx-internal-plugin-e2e` — Vitest (`node`)
  e2e harness for `libs/nx/internal-plugin`; currently has **no specs**
  (`passWithNoTests`), so a green run proves nothing.
- `libs/console/legacy-ce` 📄 → `console-legacy-ce` → `@hasura/console-legacy-ce`
  — **this is where almost all the actual UI source and tests live**. Older
  shared code under `src/lib/components/` (e.g. `components/Common/...`), newer
  code under `src/lib/features/` as feature folders (30, plus a shared
  `features/components/`: `Data`,
  `Permissions`, `RestEndpoints`, `SchemaRegistry`, `ConnectDBRedesign`,
  `EETrial`, ...). Prefer a `features/<Name>` folder for new work.
- `libs/console/legacy-ee` 📄 → `console-legacy-ee` → `@hasura/console-legacy-ee`
  — EE-only code/overrides (~200 files), builds on legacy-ce.
- `libs/shared/context` 📄 → `shared-context` → `@hasura/shared/context` —
  `AppContext`/`useAppContext` (envVars, endpoints, serverVersion,
  featuresCompatibility...), `AuthContext`/`useAuthContext`
  (authenticate/logout/getHeaders), `getEndpoints()`.
- `libs/shared/types` 📄 → `shared-types` → `@hasura/shared/types`
- `libs/shared/utils` 📄 → `shared-utils` → `@hasura/shared/utils` (incl. route
  path helpers in `src/routes.ts`)
- `libs/shared/hooks` 📄 → `shared-hooks` → `@hasura/shared/hooks`
- `libs/shared/ui` 📄 → `shared-ui` → `@hasura/shared/ui` — design-system UI
  components (`Button`, `Card`, `SimpleForm`, `InputField`, `AlertProvider`,
  `ToastsHub`, `AppTheme`, theming/dark mode...).
- `libs/shared/testing` 📄 → `shared-testing` → `@hasura/shared/testing`
- `libs/shared/analytics` 📄 → `analytics` (note: no `shared-` prefix, unlike
  its siblings) → `@hasura/shared/analytics`
- `libs/metadata/api` 📄 → `metadata-api` → `@hasura/metadata/api`
- `libs/metadata/helpers` 📄 → `metadata-helpers` → `@hasura/metadata/helpers`
- `libs/metadata/data-source` 📄 → `metadata-data-source` → `@hasura/metadata/data-source`
- `libs/nx/internal-plugin` → `nx-internal-plugin` → `@hasura/internal-plugin`
  — custom Nx executors (`build-server-assets`,
  `validate-javascript-bundle-output`, `chromatic`).
- `libs/nx/storybook-addon-console-env` → `nx-storybook-addon-console-env` —
  Storybook addon panel for switching/mocking env vars. Alias
  `storybook-addon-console-env` (not for app code).
- `libs/nx/unplugin-dynamic-asset-loader` → `nx-unplugin-dynamic-asset-loader`
  — build-time plugin, alias `unplugin-dynamic-asset-loader`.
- `libs/open-api-to-graphql` → `open-api-to-graphql` → `@hasura/open-api-to-graphql`.

Nx project names don't always match folder paths 1:1 (see `analytics` above);
check the `name` field in a `project.json`.

`dist/` and `tmp/` are gitignored build output (they contain stray copies of
`AGENTS.md`, `project.json`, etc.) — never edit or search them as source.

## Module boundaries

`@nx/enforce-module-boundaries` (root `eslint.config.mjs`) enforces project
tags: `scope:shared`, `scope:console`, `scope:metadata`, `scope:nx-plugins`,
plus `type:*` / `meta:*`. `scope:shared` libs may only depend on other
`scope:shared` libs. CE must never import EE — both legacy libs share the same
tags, so no lint rule catches this; only Nx's circular-dependency check would.

## State management, routing, theming

- **Global state:** plain React Context — `AppContext` and `AuthContext` from
  `@hasura/shared/context`. Provider tree is in
  `libs/console/legacy-ce/src/lib/client.tsx`: `ReactQueryProvider` (from
  `@hasura/metadata/api`) → `AppProvider` → `AppTheme` → `BrowserRouter` →
  `Router`.
- **Server state:** React Query (`@tanstack/react-query`).
- **Feature-local state:** Zustand (`features/IsFeatureEnabled/store.tsx`,
  `components/App/store.ts`, `telemetry/store.ts`) and feature-scoped contexts
  (e.g. `features/Data/context/*`, `features/Actions/context.ts`).
- **Routing:** declarative `<Routes>`/`<Route>` from `react-router` (not a data
  router). Route trees: `libs/console/legacy-{ce,ee}/src/lib/Router.tsx`, with
  per-feature route factories in `features/*/routes.tsx`.
- **Dark mode:** `libs/shared/ui/src/theme/` (`ThemeProvider`, `useAppearance`,
  `ThemeToggleMenuItem`). Persisted in localStorage (`hasura-console:appearance`),
  defaults to light. Tailwind `dark:` keys off a `.dark` class on `<html>`.
  Every new UI must look right in both themes.
- **Tailwind 4:** CSS-first config in each app's `src/css/tailwind.css`. The
  root `tailwind.config.js` is **not loaded** (`@config` is commented out), so
  custom classes defined only there (e.g. `bg-legacybg`, `text-muted`) silently
  produce no CSS — don't rely on them in new code.

## Commands

Run these from `frontend/`.

- `yarn lint` — lints all projects (`nx run-many --target=lint`). `yarn lint:quiet` for less noise.
- `yarn test:unit` — `nx run-many --target=test` (all projects).
- `yarn test:e2e` — `nx run-many --target=e2e` (both Cypress suites, plus the
  Vitest-based `nx-internal-plugin-e2e`). Needs a running `graphql-engine` — see
  the e2e apps' `AGENTS.md`.
- `yarn storybook` — runs **`console-legacy-ce`'s** storybook (not `console-ce`!), port 4400.
- `yarn start:ce` / `yarn start:ee` — dev server for the CE/EE app (`nx run console-ce:serve`),
  on ports 4200 / 5500. Point it at a running `graphql-engine` with
  `NX_PUBLIC_DATA_API_URL` etc. in a gitignored `frontend/.env` (no dev proxy;
  values reach the app as `window.__env`).
- `yarn build:ce` / `yarn build:ee` — production build (`nx run console-ce:build --outputStyle static`).
- `yarn server-build:ce` / `yarn server-build:ee` — builds + packages assets for the Haskell server/CLI to embed.
- `yarn format:write` (changed files vs `origin/main`) / `yarn format:write:all`;
  `npx nx format:check` to verify (there is no `format:check` script).
- `yarn generate-control-plane-gql-types` / `yarn generate-schema-registry-gql-types` — graphql-codegen.

Git pre-commit (`.husky/pre-commit`) runs lint-staged: `nx affected --target=lint --fix`
and `nx format:write`.

### Targets are mostly inferred

Many targets don't appear in `project.json` — Nx plugins in `nx.json` add them:
`@nx/webpack/plugin` (`build`, `serve`), `@nx/cypress/plugin` (`e2e` with a
`ci` configuration, `open-cypress`), `@nx/storybook/plugin` (`storybook`,
`build-storybook`, `static-storybook`, `test-storybook`), `@nx/eslint/plugin`
(`lint` for `libs/shared/**`, `libs/metadata/**`). Use
`npx nx show project <name>` to see a project's real targets.

No project has a `typecheck` target — for libs use
`npx tsc --noEmit -p <project>/tsconfig.lib.json` or trust IDE diagnostics; the
apps are type-checked by ForkTsChecker during `nx run console-{ce,ee}:build`. `tsc` follows
imports into other libs and checks them with this project's strictness, so
filter output to your project's path (e.g. `| grep libs/console/legacy-ee`).
`tsc` is **not clean** today (hundreds of pre-existing errors in legacy-ce,
mostly test fixtures and `open-api-to-graphql`) — make sure you add no new
errors in the files you touched rather than expecting zero.

### Unit tests

All unit tests run under **Vitest** (`@nx/vitest:test`); `yarn test:unit` runs
every project's suite once. Each project has a `vite.config.mts` or
`vitest.config.mts` (`jsdom` for React libs/apps, `node` for tooling libs); the
root `vitest.config.ts` discovers them via `test.projects` and excludes
`internal-plugin-e2e` (which runs only through its Nx `e2e` target). Tests are
`*.test.ts(x)` (dominant) with some `*.spec.ts(x)`.

Setup files: most projects use `tools/test-setup/setupTests.ts`;
`console-legacy-ce` uses its own `libs/console/legacy-ce/src/setupTests.ts`.
Both load `@testing-library/jest-dom/vitest` — jest-dom is a matcher library,
not a runner. The only Jest-named packages left are `@testing-library/jest-dom`
and `eslint-plugin-jest-dom`.

## Storybook conventions

- Root `.storybook/main.ts` is a base config (framework `@storybook/react-webpack5`,
  addons `links`/`a11y`/`docs`) meant to be extended, not run directly.
- `libs/console/legacy-ce/.storybook/main.ts` extends it, adds `stories` globs
  (legacy-ce **and** `libs/shared/ui`), and layers webpack tweaks.
- `libs/console/legacy-ce/.storybook/preview.tsx` sets up **global decorators** that every story gets automatically:
  - `AppTheme` (which renders `ToastsHub`) and `AlertProvider` from `@hasura/shared/ui`, plus an `appearance` toolbar toggle (light/dark).
  - `AppContext.Provider value={mockAppState}` (from `mockAppState.ts` in the same folder — a fully-populated fake `AppState`; also exports `mockEnvVars`).
  - `MemoryRouter` (from `react-router`) — so `useNavigate`/`useParams` etc. work without extra setup.
  - MSW via `mswLoader` (`msw-storybook-addon`).
  - A `parameters.mockdate` decorator for freezing time.
- **Don't re-wrap stories in `MemoryRouter`, `AppTheme` or the base `AppContext`** — it's redundant. Only add a story-level decorator when you need to _override_ context values (e.g. a different `envVars.consoleMode`, or `AuthContext` since that one has no global default) — see `Login.stories.tsx` for the pattern of building `Partial<AppState>` overrides on top of `mockAppState`.
- For components that use React Query, add `ReactQueryDecorator()` (from `@hasura/shared/testing`) to the story's `decorators` array rather than hand-rolling a `QueryClient`/`QueryClientProvider`. `FormDecorator` and `ConsoleTypeDecorator` live alongside it.
- MSW handlers go in `parameters.msw` as a plain array (not `parameters.msw.handlers`), using `http`/`HttpResponse`/`delay` imported from `msw` (MSW 3; never the old `rest` import).
- `play` functions must `await` every interaction, and use `fn()` spies from `storybook/test` for callback props you assert on.
- `EnvVars` (`libs/shared/types/src/env.ts`, imported as `@hasura/shared/types`) is a **discriminated union** keyed on `consoleMode`/`consoleType` (`OSSServerEnv | CloudServerEnv | ProServerEnv | ProLiteServerEnv | OSSCliEnv | CloudCliEnv | ...`). When building a mock env-vars object for a specific mode, spread from `mockEnvVars` and override **all** the discriminant fields for that mode together (e.g. both `consoleMode` and `consoleType`, plus that variant's required fields like `apiHost`/`cliUUID` for CLI) — partial overrides across variants produce confusing union-assignability errors. Note `OSSCliEnv` has no `consoleType`, and the Pro CLI variants are `CloudCliEnv` with `pro: true`.
- When adding a new UI component (especially under `libs/shared/ui`), suggest adding a co-located `ComponentName.stories.tsx` alongside it, following an existing sibling's stories as a template (e.g. `Default`, a long-content/scrollable variant, and any prop toggles worth previewing). Don't add one silently — offer it and let the user confirm, since not every component (e.g. a thin one-off wrapper) needs one.

## Lint conventions

- Flat config only: root `eslint.config.mjs` plus one per project.
- `react/forbid-dom-props` bans raw analytics attributes (`data-analytics-name`,
  `data-trackid`, `data-heap-*`) — use the `@hasura/shared/analytics` helpers.
- Keep `data-testid`s stable: Cypress e2e specs depend on them (e.g. the
  `Data Manager` heading).

## Before finishing

From `frontend/`, for every project you touched: `npx nx run <project>:lint`,
`npx nx run <project>:test`, a `tsc --noEmit` against its tsconfig, and
`npx nx format:check`. Per-package `AGENTS.md` files list exact commands.
