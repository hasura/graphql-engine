import { defineConfig } from 'vitest/config';
import react from '@vitejs/plugin-react';
import { nxViteTsPaths } from '@nx/vite/plugins/nx-tsconfig-paths.plugin';
import { nxCopyAssetsPlugin } from '@nx/vite/plugins/nx-copy-assets.plugin';
import path from 'path';

const stubbedExtensions =
  /\.(css|less|scss|sass|jpg|ico|jpeg|png|gif|eot|otf|webp|svg|ttf|woff|woff2|mp4|webm|wav|mp3|m4a|aac|oga)$/;

export default defineConfig(() => ({
  root: __dirname,
  cacheDir: '../../../node_modules/.vite/libs/console/legacy-ce',
  plugins: [react(), nxViteTsPaths(), nxCopyAssetsPlugin(['*.md'])],
  resolve: {
    alias: [
      {
        find: stubbedExtensions,
        replacement: '$&',
        customResolver: () => path.resolve(__dirname, 'src/emptyModuleStub.ts'),
      },
    ],
  },
  define: {
    __DEV__: true,
    CONSOLE_ASSET_VERSION: JSON.stringify(Date.now().toString()),
    __DEVELOPMENT__: true,
    __CLIENT__: true,
    __SERVER__: true,
    __DISABLE_SSR__: true,
    __DEVTOOLS__: true,
    socket: true,
    webpackIsomorphicTools: true,
  },
  test: {
    name: 'console-legacy-ce',
    watch: false,
    globals: true,
    environment: 'jsdom',
    env: {
      TZ: 'UTC',
    },
    setupFiles: ['src/setupTests.ts'],
    include: ['src/**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}'],
    reporters: ['default'],
    coverage: {
      reportsDirectory: '../../../coverage/libs/console/legacy-ce',
      provider: 'v8' as const,
    },
  },
}));
