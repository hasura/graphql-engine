import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    projects: [
      '**/vite.config.{mjs,js,ts,mts}',
      '**/vitest.config.{mjs,js,ts,mts}',
      '!vitest.config.{mjs,js,ts,mts}',
      '!vite.config.{mjs,js,ts,mts}',
      // E2E projects run via their own Nx `e2e` target (with a build
      // prerequisite) — keep them out of the unit-test aggregator so a bare
      // root `vitest run` never triggers the heavy Nx-plugin e2e.
      '!**/internal-plugin-e2e/vitest.config.{mjs,js,ts,mts}',
    ],
  },
});
