# AGENTS.md (apps/nx/internal-plugin-e2e)

Nx project `nx-internal-plugin-e2e` (tags `scope:nx-plugins`, `type:e2e`;
implicit dep on `nx-internal-plugin`, i.e. `libs/nx/internal-plugin`).

## Current state: no tests

`tests/` holds only `utils.ts` (`createUtilsForLibTesting`). It wraps
`@nx/plugin/testing` (`runNxCommandAsync`, `updateFile`, `readFile`) to
generate `@nx/react` libs in a temp workspace and collect
`@nx/enforce-module-boundaries` lint errors. The only spec,
`tests/nx-internal-plugin.spec.ts`, and its snapshot were deleted in
0e19738992 ("fix build"). It provisioned a fresh Nx workspace and checked the
scope/type tag lint rules. The plugin's executors (`build-server-assets`,
`validate-javascript-bundle-output`, `chromatic`) were never covered here.
With `passWithNoTests: true`, the target passes while running nothing.

## Config

- Runner: Vitest (`@nx/vitest:test`), `vitest.config.mts`, `environment:
'node'`, globals on, `include: tests/**/*.{test,spec}.*`, test and hook
  timeouts of 320s (a spec provisions a real workspace).
- `e2e` `dependsOn: ["nx-internal-plugin:build"]`. The root
  `frontend/vitest.config.ts` excludes this config on purpose (075d23c21a),
  so `yarn test:unit` and a bare `vitest run` never run it.

## Running / checks (from `frontend/`)

- `npx nx e2e nx-internal-plugin-e2e`
- `npx nx lint nx-internal-plugin-e2e`
