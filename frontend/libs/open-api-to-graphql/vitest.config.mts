import { defineConfig } from 'vitest/config';
import { nxViteTsPaths } from '@nx/vite/plugins/nx-tsconfig-paths.plugin';
import { nxCopyAssetsPlugin } from '@nx/vite/plugins/nx-copy-assets.plugin';

export default defineConfig(() => ({
  root: __dirname,
  cacheDir: '../../node_modules/.vite/libs/open-api-to-graphql',
  plugins: [nxViteTsPaths(), nxCopyAssetsPlugin(['*.md'])],
  test: {
    name: 'open-api-to-graphql',
    watch: false,
    globals: true,
    passWithNoTests: true,
    environment: 'node',
    include: [
      '{src,test,tests}/**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}',
    ],
    reporters: ['default'],
    coverage: {
      // Coverage is enabled only for the `test:ci` configuration (nx passes
      // `coverage: true`); these reporters restore the file output (lcov/html)
      // the previous Jest `codeCoverage` CI config produced.
      reportsDirectory: '../../coverage/libs/open-api-to-graphql',
      provider: 'v8' as const,
      reporter: ['text', 'html', 'lcov'],
    },
  },
}));
