import { defineConfig } from 'cypress';

import * as customTasks from './cypress/support/tasks';

export default defineConfig({
  env: {
    BASE_URL: 'http://localhost:3000',
    TEST_MODE: 'parallel',
    MIGRATE_URL: 'http://localhost:9693/apis/migrate',
  },
  viewportWidth: 1280,
  viewportHeight: 720,
  chromeWebSecurity: false,
  video: false,
  projectId: '5yiuic',
  retries: 1,
  numTestsKeptInMemory: 10,
  e2e: {
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
      'cypress/e2e/**/*test.{js,jsx,ts,tsx}',
      'cypress/support/**/*unit.test.{js,ts}',
    ],
  },
});
