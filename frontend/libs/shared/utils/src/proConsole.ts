import type { EnvVars } from '@hasura/shared/types';

export const isProConsole = (env: EnvVars) => {
  if (
    env.consoleType === 'cloud' ||
    env.consoleType === 'pro' ||
    (env.consoleType === 'pro-lite' && env.projectID)
  ) {
    return true;
  }

  if (env.consoleMode === 'cli' && env.pro === true) return true;

  return false;
};

export const isMonitoringTabSupportedEnvironment = (env: EnvVars) => {
  // pro-lite and OSS environments won't have access to metrics server
  if (env.consoleMode === 'server')
    return env.consoleType === 'cloud' || env.consoleType === 'pro';
  // cloud and current self hosted setup will have pro:true
  else if (env.consoleMode === 'cli') return env.pro === true;

  // there should not be any other console modes
  return false;
};

// isProConsole or isProLiteConsole
export const isCachingEnabled = (env: EnvVars) =>
  isProConsole(env) || env.consoleType === 'pro-lite';

export const isOpenTelemetrySupported = (env: EnvVars) =>
  isProConsole(env) || env.consoleType === 'pro-lite';

/*
 * This function returns true only if the current context is Hasura Cloud
 * consoleType === 'cloud' is not enough because
 * consoleType === 'cloud' is also true for Hasura EE
 * */
export function isCloudConsole(env: EnvVars) {
  return !!env.tenantID && env.consoleType === 'cloud';
}

export function isEECloud(env: EnvVars) {
  return !env.tenantID && env.consoleType === 'cloud';
}

export function getProjectId(env: EnvVars) {
  return isCloudConsole(env) ? env.projectID : undefined;
}

export function getTenantId(env: EnvVars) {
  return isCloudConsole(env) ? env.tenantID : undefined;
}
