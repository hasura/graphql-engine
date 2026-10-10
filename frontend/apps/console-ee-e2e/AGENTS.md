# AGENTS.md (apps/console-ee-e2e)

Nx project `console-ee-e2e` (tags `scope:console`, `type:e2e`,
`meta:legacy`; implicit dep on `console-ee`). Cypress 15 e2e suite for the
EE console. Same setup as `../console-ce-e2e/AGENTS.md` (read it first);
only the differences are listed here.

## Targets / config

- `project.json` declares only `lint`. `e2e` (with `ci` = `--record
--parallel`) and `open-cypress` are inferred by `@nx/cypress/plugin`.
- `cypress.config.ts`: `nxE2EPreset` starts or reuses `npx nx run
console-ee:serve` (5-min timeout). `baseUrl: http://localhost:5500` (EE
  webpack dev-server port, from `apps/console-ee/webpack.config.js`).
  `projectId: '672jmv'`, `retries.runMode: 1`, viewport 1440x900.
- `specPattern` is the **preset default**, `src/**/*.cy.{js,jsx,ts,tsx}`, so
  EE specs must end in `.cy.ts`. This differs from CE, where specs end in
  `test.ts`.
- Node tasks: `src/support/tasks/index.ts` (`readFileMaybe`,
  `readSnapshot`, `writeSnapshot`; relative imports need `.ts` extensions).

## src layout

- `src/e2e/smokeTest.e2e.cy.ts`: the only active spec. It checks that every
  main tab loads, which catches circular imports that would blank the console.
- `src/e2e/data/dynamicDbRouting/dynamicDbRouting.e2e.cy.ts`: currently
  `xdescribe` (skipped), with a `__snapshots__/` baseline.
- `src/utils/checkMetadataPayload.ts` (`readMetadata`): metadata helper.
- `src/support/`: `endpoints.ts` (copy of CE's: `hgeUrl`/`cliUrl`, env
  `HGE_URL`/`CLI_URL`, defaults :8080 / :9693), `snapshots.ts`
  (`toMatchSnapshot`, same strict rules as CE: missing snapshot fails, write
  only with `--env updateSnapshots=true`), `commands.ts` (only
  `@testing-library/cypress` + snapshots).
- `src/support/index.d.ts` declares `clearConsoleTextarea` and
  `expect*Notification*`, but EE does **not** implement them. Only CE does.
  Port them from `../console-ce-e2e/src/support/` before using them.
- `src/fixtures/example.json`: an unused placeholder.

## Selectors

The smoke test depends on these exact testids: `Nav bar`, `Data Manager`,
`data-create-actions`, `data-create-remote-schemas`, `data-create-trigger`,
`data-export-metadata`, and `[data-test="graphiql-explorer-link"]`. Don't
remove or rename them in `libs/console/*`. `Data Manager` was already
restored once (a4322402d5).

## Backend

CI (`.buildkite/scripts/frontend/test-ee-console.sh`) runs HGE on :8080 and
the CLI console on :9693, with `NX_PUBLIC_CONSOLE_MODE=cli` and
`NX_PUBLIC_HASURA_CONSOLE_TYPE=pro`. Locally, the dev server reads
`frontend/.env`.

## Running / checks (from `frontend/`)

- `npx nx lint console-ee-e2e`
- `npx tsc --noEmit -p apps/console-ee-e2e/tsconfig.json`
- `npx nx e2e console-ee-e2e --skip-nx-cache` (needs HGE; starts or reuses
  `console-ee:serve` on :5500)
- Interactive: `npx nx open-cypress console-ee-e2e`
