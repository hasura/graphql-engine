import globals from '../../../Globals';
import { useEELicenseInfo } from './useEELicenseInfo';
import { EELiteAccess } from '../types';
import { transformEntitlementToAccess } from '../utils';

export const useEELiteAccess = (g: typeof globals): EELiteAccess => {
  const { data, error, isLoading } = useEELicenseInfo({
    enabled: g.consoleType === 'pro-lite',
  });

  if (isLoading) {
    return {
      access: 'loading',
    };
  }

  if (error || !data) {
    return {
      access: 'forbidden',
    };
  }

  return transformEntitlementToAccess(data);
};
