import { defineConfig } from 'vitest/config';
import react from '@vitejs/plugin-react';
import { nxViteTsPaths } from '@nx/vite/plugins/nx-tsconfig-paths.plugin';
import { nxCopyAssetsPlugin } from '@nx/vite/plugins/nx-copy-assets.plugin';
export default defineConfig(() => ({
  root: __dirname,
  cacheDir: '../../../node_modules/.vite/libs/metadata/data-source',
  plugins: [react(), nxViteTsPaths(), nxCopyAssetsPlugin(['*.md'])],
  // Uncomment this if you are using workers.
  // worker: {
  //   plugins: () => [ nxViteTsPaths() ],
  // },
  test: {
    name: 'data-source',
    watch: false,
    globals: true,
    environment: 'jsdom',
    // Jest's jsdom environment defaulted `testURL`/`testEnvironmentOptions.url`
    // to `http://localhost/` (no port). Vitest's jsdom environment defaults to
    // `http://localhost:3000/` instead. Tests here build API endpoints from a
    // hard-coded `http://localhost` base (see `testWrapper` in
    // `@hasura/shared/testing`) and mock them with MSW using relative paths,
    // which MSW resolves against `window.location.href`. Without pinning the
    // origin back to Jest's old default, those relative mocks resolve to
    // `http://localhost:3000/...` and never match the app's real
    // `http://localhost/...` requests.
    environmentOptions: {
      jsdom: {
        url: 'http://localhost/',
      },
    },
    include: ['src/**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}'],
    // Shared setup: jest-dom matchers, ResizeObserver + canvas (lottie) jsdom stubs.
    setupFiles: ['../../../tools/test-setup/setupTests.ts'],
    reporters: ['default'],
    coverage: {
      reportsDirectory: '../../../coverage/libs/metadata/data-source',
      provider: 'v8' as const,
    },
  },
}));
