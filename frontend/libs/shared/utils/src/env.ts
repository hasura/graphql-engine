import { ConsoleType } from '@hasura/shared/types';

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
