import { useQuery } from '@tanstack/react-query';
import {
  EE_LICENSE_INFO_QUERY_NAME,
  LICENSE_REFRESH_INTERVAL,
} from '../constants';
import { useAppContext } from '@hasura/shared/context';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { EELicenseInfo } from '../types';

export const useEELicenseInfo = (opts?: { enabled: boolean }) => {
  const fetchJson = useAuthFetchJson();
  const { endpoints } = useAppContext();

  return useQuery({
    queryKey: EE_LICENSE_INFO_QUERY_NAME,
    queryFn: async () => {
      return fetchJson(endpoints.entitlement, {
        method: 'GET',
      }).then((resp: any) => {
        const licenseInfo: EELicenseInfo = {
          status: resp.status,
          type: resp.type,
          expiry_at: resp.expiry_at ? new Date(resp.expiry_at) : undefined,
          grace_at: resp.grace_at ? new Date(resp.grace_at) : undefined,
        };

        return licenseInfo;
      });
    },
    refetchOnMount: false,
    refetchOnWindowFocus: false,
    staleTime: LICENSE_REFRESH_INTERVAL,
    enabled: opts?.enabled !== false,
  });
};
