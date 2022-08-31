import { defineConfig } from 'cypress';
import { nxE2EPreset } from '@nrwl/cypress/plugins/cypress-preset';

import * as customTasks from './src/support/tasks';

const nxConfig = nxE2EPreset(__dirname);

export default defineConfig({
  env: {
    TEST_MODE: 'parallel',
    MIGRATE_URL: 'http://localhost:9693/apis/migrate',
  },
  viewportWidth: 1280,
  viewportHeight: 720,
  chromeWebSecurity: false,
  video: false,
  projectId: '5yiuic',
  numTestsKeptInMemory: 10,
  e2e: {
    ...nxConfig,

    // We've imported your old cypress plugins here.
    // You may want to clean this up later by importing these.
    setupNodeEvents(on, config) {
      on('task', {
        ...customTasks,
      });

      return config;
    },
    baseUrl: 'http://localhost:3000',
    specPattern: [
      'src/e2e/**/*test.{js,jsx,ts,tsx}',
      'src/support/**/*unit.test.{js,ts}',
    ],
  },
});
