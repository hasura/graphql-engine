import { defineConfig } from 'cypress';
import { nxE2EPreset } from '@nrwl/cypress/plugins/cypress-preset';

import * as customTasks from './src/support/tasks';

type ConfigOptions = Parameters<typeof defineConfig>[0];

const nxConfig = nxE2EPreset(__dirname);

interface MyConfigOptions extends ConfigOptions {
  useRelativeSnapshots?: boolean;
}

const myDefineConfig = (config: MyConfigOptions) => defineConfig(config);
export default myDefineConfig({
  viewportWidth: 1440,
  viewportHeight: 900,

  chromeWebSecurity: false,
  numTestsKeptInMemory: 10,

  retries: {
    openMode: 0,
    // Allows for one automatic retry per test
    // see: https://docs.cypress.io/guides/guides/test-retries#How-It-Works
    runMode: 1,
  },

  projectId: '5yiuic',

  e2e: {
    ...nxConfig,

    video: false,
    baseUrl: 'http://localhost:4200',
    specPattern: [
      'src/e2e/**/*test.{js,jsx,ts,tsx}',
      'src/support/**/*unit.test.{js,ts}',
    ],

    setupNodeEvents(on, config) {
      on('task', {
        ...customTasks,
      });

      return config;
    },
  },

  useRelativeSnapshots: true,
});
