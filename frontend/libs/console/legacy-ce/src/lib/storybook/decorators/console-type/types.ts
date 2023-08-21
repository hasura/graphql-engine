export type ConsoleTypes = 'oss' | 'pro-lite' | 'pro' | 'cloud' | 'cloud-pro';

export type EnvState = Partial<typeof window.__env> & {
  adminSecret: string | undefined;
  consoleType: ConsoleTypes;
};

export type EnvStateArgs = {
  consoleType: ConsoleTypes;
  adminSecret?: boolean;
};

export type MenuOptions = { menuPlacement?: 'top' | 'bottom' };

export type StorybookGlobals = {
  consoleType: ConsoleTypes;
  isAdminSecretSet: boolean;
  adminSecret: string | undefined;
  hasuraCloudTenantId: string | undefined;
};
