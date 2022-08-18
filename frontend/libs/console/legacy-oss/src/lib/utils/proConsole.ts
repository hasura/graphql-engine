import type { CloudCliEnv, EnvVars } from '@/Globals';

export type ProConsoleEnv = {
  consoleMode: EnvVars['consoleMode'];
  consoleType?: EnvVars['consoleType'];
  pro?: CloudCliEnv['pro'];
};

export const isProConsole = (env: ProConsoleEnv) => {
  if (
    env.consoleMode === 'server' &&
    (env.consoleType === 'cloud' ||
      env.consoleType === 'pro' ||
      env.consoleType === 'pro-lite')
  ) {
    return true;
  }

  if (env.consoleMode === 'cli' && env.pro === true) {
    return true;
  }

  return false;
};
