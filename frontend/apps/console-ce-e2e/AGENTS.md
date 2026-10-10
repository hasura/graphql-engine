# AGENTS.md (apps/console-ce-e2e)

Nx project `console-ce-e2e` (tags `scope:console`, `type:e2e`,
`meta:legacy`; implicit dep on `console-ce`). Cypress 15 e2e suite for the
CE console, run against a **real** graphql-engine (HGE). See also
`../console-ee-e2e/AGENTS.md` (same setup, much smaller suite).

## Targets (inferred by `@nx/cypress/plugin` in `nx.json`)

`project.json` only declares `lint`. `e2e` (`cypress run`, `dependsOn
^build`, cached), its `ci` configuration (adds `--record --parallel`), and
`open-cypress` (`cypress open`) are inferred from `cypress.config.ts`. There
is no `devServerTarget` and no `e2e-ci` target any more (commit 7e42f4673d).

## cypress.config.ts

- `nxE2EPreset(__filename, { cypressDir: 'src', webServerCommands: { default:
'npx nx run console-ce:serve' }, webServerConfig: { timeout: 5 min } })`.
  The preset starts the dev server from `setupNodeEvents`, or **reuses** one
  already answering on `baseUrl`. Keep the `await
nxConfig.setupNodeEvents(...)` call when editing `setupNodeEvents`.
- `baseUrl: http://localhost:4200` (the `console-ce:serve` webpack port);
  `support/getBaseUrl.ts` throws if it is empty.
- `specPattern` is **custom**: `src/e2e/**/*test.{js,jsx,ts,tsx}` +
  `src/support/**/*unit.test.{js,ts}`. Files named `*.spec.ts` are NOT run;
  here they are helper modules (`data/manage-database/postgres.spec.ts`,
  `settings/metadata/spec.ts`). New specs must end in `test.ts`.
- `retries.runMode: 1`, viewport 1440x900, `chromeWebSecurity: false`,
  `projectId: '5yiuic'`. Node tasks come from `src/support/tasks/index.ts`
  (`readFileMaybe`, `readSnapshot`, `writeSnapshot`). Relative imports in
  the config/tasks need explicit `.ts` extensions (loaded as native ESM;
  `allowImportingTsExtensions` in `tsconfig.json`).

## Backend endpoints

- The test code's own HGE/CLI requests go through `src/support/endpoints.ts`
  (`hgeUrl('/v1/metadata')`, `cliUrl('/apis/migrate')`). Defaults
  `http://localhost:8080` / `http://localhost:9693`; override with
  `CYPRESS_HGE_URL` / `CYPRESS_CLI_URL` or `--env HGE_URL=...,CLI_URL=...`.
  Never hardcode `localhost:8080` in a spec. Don't use these helpers for
  unrelated URLs (e.g. a remote-schema endpoint).
- The **console under test** reads its own `NX_PUBLIC_*` env (from
  `frontend/.env`, e.g. `NX_PUBLIC_DATA_API_URL=http://localhost:8080`,
  `NX_PUBLIC_CONSOLE_MODE=server`). `HGE_URL` does not change that; point
  both at the same HGE. CI (`.buildkite/scripts/frontend/test-oss-console.sh`)
  runs HGE with `--enable-remote-schema-permissions` and no admin secret,
  plus the Hasura CLI console on :9693, with `NX_PUBLIC_CONSOLE_MODE=cli`,
  `NX_PUBLIC_HASURA_CONSOLE_TYPE=oss`.

## src layout

- `src/e2e/<feature>/`: one folder per console area (`actions/`,
  `cron-triggers/`, `event-triggers/`, `one-off-scheduled-triggers/`,
  `remote-schemas/`, `settings/`, `table-permission-input-validation/`,
  `tracing/`, `data/`), plus `smokeTest.e2e.test.ts`. Larger features keep
  local `fixtures/`, `utils/{requests,services,testState}`, `helpers/`.
- Naming: `*.e2e.test.ts` / plain `*.test.ts` hit the real HGE;
  `*.integration.test.ts` stub server requests with `cy.intercept` fixtures.
- `src/e2e/utils/checkMetadataPayload.ts`: shared metadata assertion.
- `src/support/`: `e2e.ts` -> `commands.ts` (imports
  `@testing-library/cypress`, `snapshots`, `clearConsoleTextarea`,
  `notifications`, `radixSelect`); types in `index.d.ts`.
- Custom commands: `toMatchSnapshot`, `clearConsoleTextarea`,
  `expect{Success,Error}Notification[WithTitle|WithMessage]`, and
  `radixSelect(label)`. Chain `radixSelect` off the hidden native
  `<select name=...>` of a Radix `SelectField`; `cy.select()` doesn't work
  there.

## Selectors

Use `cy.findByTestId(...)` or `[data-testid=...]` (most common), some legacy
`[data-test=...]`. Specs depend on exact app testids, e.g. the smoke test
needs `data-testid="Data Manager"` (restored in a4322402d5) and track-table
hooks need `[data-testid="track-public.user_table"]`. **Don't remove or
rename `data-testid`/`data-test` attributes in `libs/console/*`** without
grepping both e2e apps. Fresh projects show an onboarding popup; specs hide it
by setting `console:showConsoleOnboarding` in `onBeforeLoad`.

## Snapshots

`cy.wrap(value).toMatchSnapshot()` (`src/support/snapshots.ts`) writes JSON
to a sibling `__snapshots__/<spec>.snap`, keyed `<titlePath> #<n>`, with keys
sorted. A missing snapshot or a null/undefined subject **fails**. Create or
update snapshots only with `--env updateSnapshots=true`, then commit them.

Recent fixes on this branch (298b16c135, 16146682c3, a73a3384da) followed the
Data manager move to `/data/manage/source/:source`, the switch to Radix
selects, and real app bugs such as the cron `schedule` key. When a spec
fails, fix the app or the selector. Don't weaken assertions.

## Running / checks (from `frontend/`)

- CI-like local run: `docker/e2e/run-e2e.sh [ce|ee] [--keep] [-- --spec <path>]`
  starts Postgres + graphql-engine + `hasura console` (CLI mode) from
  `docker/e2e/docker-compose.yml`, serves the current checkout with
  `nx run console-ce:serve` (:4200; EE `console-ee:serve` on :5500) using CI's
  `NX_PUBLIC_*` env (overriding `frontend/.env`), runs `cypress run` against it,
  then stops the server and tears the stack down. `HGE_VERSION`, `HGE_PORT`,
  `CLI_API_PORT` override the image tag and host ports. Refuses to start if
  something already listens on the console port. Logs: `tmp/e2e-logs/`.
- `npx nx lint console-ce-e2e`
- `npx tsc --noEmit -p apps/console-ce-e2e/tsconfig.json`
- `npx nx e2e console-ce-e2e --skip-nx-cache` (needs HGE on :8080 and the
  CLI on :9693, or the env overrides above; starts or reuses `console-ce:serve`)
- Single spec: `npx nx e2e console-ce-e2e --skip-nx-cache --spec
src/e2e/smokeTest.e2e.test.ts` (path is relative to `apps/console-ce-e2e`)
- Interactive: `npx nx open-cypress console-ce-e2e`
- Testing guide: `frontend/docs/dev/testing/3-testing-best-practices.stories.mdx`
  (the link in `README.md` is stale).
