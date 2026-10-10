import { defineConfig } from 'vitest/config';
import { nxViteTsPaths } from '@nx/vite/plugins/nx-tsconfig-paths.plugin';

export default defineConfig(() => ({
  root: __dirname,
  cacheDir: '../../../node_modules/.vite/nx-unplugin-dynamic-asset-loader',
  plugins: [nxViteTsPaths()],
  test: {
    name: 'nx-unplugin-dynamic-asset-loader',
    watch: false,
    globals: true,
    environment: 'node',
    include: ['src/**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}'],
    reporters: ['default'],
    coverage: {
      reportsDirectory: '../../../coverage/nx-unplugin-dynamic-asset-loader',
      provider: 'v8' as const,
    },
  },
}));
