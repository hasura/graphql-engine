import { defineConfig } from 'vitest/config';
import { nxViteTsPaths } from '@nx/vite/plugins/nx-tsconfig-paths.plugin';

export default defineConfig(() => ({
  root: __dirname,
  cacheDir: '../../../node_modules/.vite/apps/nx/internal-plugin-e2e',
  plugins: [nxViteTsPaths()],
  test: {
    name: 'nx-internal-plugin-e2e',
    watch: false,
    globals: true,
    environment: 'node',
    include: ['tests/**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}'],
    reporters: ['default'],
    // The e2e provisions a real Nx workspace + installs the built plugin and runs
    // generators; keep the generous timeouts the suite already declares per hook/test.
    testTimeout: 320_000,
    hookTimeout: 320_000,
    passWithNoTests: true,
    coverage: {
      reportsDirectory: '../../../coverage/apps/nx/internal-plugin-e2e',
      provider: 'v8' as const,
    },
  },
}));
