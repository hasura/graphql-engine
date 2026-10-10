import { differenceInDays } from 'date-fns';
import { EELicenseInfo, EELiteAccess } from './types';
import { createControlPlaneClient } from '../ControlPlane';
import endpoints from '../../Endpoints';

export const getExpiryDetails = (
  expiry_at: Date,
  grace_at?: Date,
): {
  status: 'grace' | 'expired';
  expiresAt: Date;
} => {
  const expiry = grace_at ?? expiry_at;
  const status = grace_at
    ? grace_at.getTime() > new Date().getTime()
      ? 'grace'
      : 'expired'
    : 'expired';

  return {
    status,
    expiresAt: expiry,
  };
};

export const getDaysFromNow = (refDate: Date) => {
  return differenceInDays(new Date(), refDate);
};

export const eeTrialsLuxDataEndpoint = endpoints.registerEETrial;

export const eeTrialsControlPlaneClient = createControlPlaneClient(
  eeTrialsLuxDataEndpoint,
  {
    'x-hasura-role': 'public',
  },
);

export const transformEntitlementToAccess = (
  data: EELicenseInfo,
): EELiteAccess => {
  switch (data.status) {
    case 'active': {
      return {
        access: 'active',
        license: data,
        expires_at: new Date(data.expiry_at),
        kind: 'default',
      };
    }
    case 'expired': {
      const { status, expiresAt } = getExpiryDetails(
        data.expiry_at,
        data.grace_at,
      );
      if (status === 'grace') {
        return {
          access: 'active',
          license: data,
          expires_at: expiresAt,
          kind: 'grace',
        };
      } else {
        return {
          access: 'expired',
          license: data,
        };
      }
    }
    case 'deactivated': {
      return {
        access: 'deactivated',
        license: data,
      };
    }
    case 'none':
    default: {
      return {
        access: 'eligible',
      };
    }
  }
};
