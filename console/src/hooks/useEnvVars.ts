import globals from '@/Globals';

export interface ConsoleConfig {
  mode: 'server' | 'cli';
  is_admin_secret_set: boolean;
  type: string;
}

const getConsoleType = (type: string) => {
  if (type === 'undefined') return 'oss';
  return type;
};

export const useConsoleConfig = (): ConsoleConfig => {
  return {
    mode: globals.consoleMode as 'server' | 'cli',
    is_admin_secret_set: Boolean(globals.isAdminSecretSet),
    type: getConsoleType(globals.consoleType),
  };
};
