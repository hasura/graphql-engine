import { Api } from '../../hooks/apiUtils';
import moment from 'moment';
import { EELicenseInfo, EELiteAccess } from './types';
import { reactQueryClient } from '../../lib/reactQuery';
import {
  EE_LICENSE_INFO_QUERY_NAME,
  LICENSE_REFRESH_INTERVAL,
} from './constants';
import Endpoints from '../../Endpoints';
import { createControlPlaneClient } from '../ControlPlane';
import endpoints from '../../Endpoints';

export const fetchEELicenseInfo = (headers: Record<string, string>) => {
  return Api.get<any, EELicenseInfo>(
    {
      headers,
      url: Endpoints.entitlement,
    },
    (resp: any) => {
      const licenseInfo: EELicenseInfo = {
        status: resp.status,
        type: resp.type,
        expiry_at: resp.expiry_at ? new Date(resp.expiry_at) : undefined,
        grace_at: resp.grace_at ? new Date(resp.grace_at) : undefined,
      };
      return licenseInfo;
    }
  );
};

export const prefetchEELicenseInfo = (headers: Record<string, string>) => {
  reactQueryClient.prefetchQuery({
    queryKey: EE_LICENSE_INFO_QUERY_NAME,
    queryFn: () => {
      return fetchEELicenseInfo(headers);
    },
    staleTime: LICENSE_REFRESH_INTERVAL,
  });
};

export const getExpiryDetails = (
  expiry_at: Date,
  grace_at?: Date
): {
  status: 'grace' | 'expired';
  expiresAt: moment.Moment;
} => {
  const expiry = grace_at ? moment(grace_at) : moment(expiry_at);
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
  const momentRef = moment(refDate);
  const momentNow = moment(new Date());
  return momentNow.diff(momentRef, 'days');
};

export const eeTrialsLuxDataEndpoint = endpoints.registerEETrial;

export const eeTrialsControlPlaneClient = createControlPlaneClient(
  eeTrialsLuxDataEndpoint,
  {
    'x-hasura-role': 'public',
  }
);

export const transformEntitlementToAccess = (
  data: EELicenseInfo
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
        data.grace_at
      );
      if (status === 'grace') {
        return {
          access: 'active',
          license: data,
          expires_at: expiresAt.toDate(),
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
