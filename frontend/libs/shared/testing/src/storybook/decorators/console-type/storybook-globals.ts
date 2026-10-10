import { StorybookGlobals } from './types';

// are used in place of globals via Proxy when running storybook
// the way it works is that any property that is declared here will be used in place of the regular globals object
// so we can build on this over time simply by adding properties
export let storybookGlobals: StorybookGlobals = {
  consoleType: 'oss',
  adminSecret: undefined,
  isAdminSecretSet: false,
  hasuraCloudTenantId: undefined,
};

// need an updater function as you can't mutate an import in other modules
export const updateStorybookGlobals = (updated: StorybookGlobals) => {
  storybookGlobals = { ...updated };
};
