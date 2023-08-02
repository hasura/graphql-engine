import globals from '../Globals';

export interface ConsoleConfig {
  mode: 'server' | 'cli';
  is_admin_secret_set: boolean;
  type: string;
}

export const getEnvVarAsString = (value: string) => {
  if (!value || value === 'undefined') return undefined;
  return value;
};

const getEnvVarAsBoolean = (value: string | boolean) => {
  if (typeof value === 'boolean') return value;

  return value === 'true';
};

export const useConsoleConfig = (): ConsoleConfig => {
  return {
    mode: globals.consoleMode as 'server' | 'cli',
    is_admin_secret_set: getEnvVarAsBoolean(globals.isAdminSecretSet),
    type: getEnvVarAsString(globals.consoleType ?? '') ?? 'oss',
  };
};
