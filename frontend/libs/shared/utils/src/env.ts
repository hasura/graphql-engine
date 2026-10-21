import { ConsoleType, EnvVars } from '@hasura/shared/types';
import { isEmpty } from './data';

export function parseConsoleType(envConsoleType: unknown): ConsoleType {
  switch (envConsoleType) {
    case 'oss':
    case 'cloud':
    case 'pro':
    case 'pro-lite':
      return envConsoleType;

    default:
      throw new Error(`Unmanaged console type "${envConsoleType}"`);
  }
}

export const getEnvVarAsString = (value: string) => {
  if (!value || value === 'undefined') return undefined;
  return value;
};

export const getEnvVarAsBoolean = (value: string | boolean) => {
  if (typeof value === 'boolean') return value;

  return value === 'true';
};

/**
 * Whether the engine is protected by an admin secret. The server sets
 * `isAdminSecretSet`, but the CLI only injects `adminSecret`, so either one
 * counts.
 */
export const hasAdminSecret = (
  envVars: Pick<EnvVars, 'isAdminSecretSet' | 'adminSecret'>,
) => Boolean(envVars.isAdminSecretSet) || !isEmpty(envVars.adminSecret);
