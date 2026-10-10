import { defineConfig } from 'vitest/config';
import react from '@vitejs/plugin-react';
import { nxViteTsPaths } from '@nx/vite/plugins/nx-tsconfig-paths.plugin';

export default defineConfig(() => ({
  root: __dirname,
  cacheDir: '../../node_modules/.vite/console-ee',
  plugins: [react(), nxViteTsPaths()],
  test: {
    name: 'console-ee',
    watch: false,
    globals: true,
    environment: 'jsdom',
    setupFiles: ['../../tools/test-setup/setupTests.ts'],
    include: [
      '{src,test,tests}/**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}',
    ],
    reporters: ['default'],
    passWithNoTests: true,
    coverage: {
      reportsDirectory: '../../coverage/console-ee',
      provider: 'v8' as const,
    },
  },
}));
