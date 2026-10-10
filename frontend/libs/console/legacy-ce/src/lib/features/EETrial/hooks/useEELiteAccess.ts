import { useEELicenseInfo } from './useEELicenseInfo';
import { EELiteAccess } from '../types';
import { transformEntitlementToAccess } from '../utils';
import { useAppContext } from '@hasura/shared/context';
import { parseConsoleType } from '@hasura/shared/utils';
import { ConsoleType } from '@hasura/shared/types';

export const useEELiteAccess = (): EELiteAccess & {
  consoleType: ConsoleType;
} => {
  const { envVars } = useAppContext();
  const { data, error, isLoading } = useEELicenseInfo({
    enabled: envVars.consoleType === 'pro-lite',
  });
  const consoleType = envVars?.consoleType
    ? parseConsoleType(envVars.consoleType)
    : 'oss';

  if (isLoading) {
    return {
      access: 'loading',
      consoleType,
    };
  }

  if (error || !data) {
    return {
      access: 'forbidden',
      consoleType,
    };
  }

  return {
    ...transformEntitlementToAccess(data),
    consoleType,
  };
};
