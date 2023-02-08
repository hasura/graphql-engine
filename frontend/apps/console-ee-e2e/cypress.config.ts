import { defineConfig } from 'cypress';
import { nxE2EPreset } from '@nrwl/cypress/plugins/cypress-preset';

const nxConfig = nxE2EPreset(__dirname);

export default defineConfig({
  viewportWidth: 1440,
  viewportHeight: 900,

  retries: {
    openMode: 0,
    // Allows for one automatic retry per test
    // see: https://docs.cypress.io/guides/guides/test-retries#How-It-Works
    runMode: 1,
  },

  projectId: '672jmv',

  e2e: {
    ...nxConfig,

    video: false,
  },
});
