import { EnvStateArgs, EnvState } from './types';

// creates a partial of what goes on window.__env
export const createEnvState = ({
  consoleType,
  adminSecret,
}: EnvStateArgs): EnvState => {
  return {
    consoleType: consoleType === 'cloud-pro' ? 'cloud' : consoleType,
    isAdminSecretSet: adminSecret,
    adminSecret: adminSecret ? '*******' : undefined,
    tenantID: consoleType === 'cloud-pro' ? 'tenant-id' : undefined, // globals.hasuraCloudTenantId
  };
};
