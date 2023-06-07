import { useQuery } from 'react-query';
import { useAppSelector } from '../../../storeHooks';
import { fetchEELicenseInfo } from '../utils';
import {
  EE_LICENSE_INFO_QUERY_NAME,
  LICENSE_REFRESH_INTERVAL,
} from '../constants';

export const useEELicenseInfo = (opts?: { enabled: boolean }) => {
  const headers = useAppSelector(state => state.tables.dataHeaders);
  return useQuery({
    queryKey: EE_LICENSE_INFO_QUERY_NAME,
    queryFn: () => {
      return fetchEELicenseInfo(headers);
    },
    refetchOnMount: false,
    refetchOnWindowFocus: true,
    staleTime: LICENSE_REFRESH_INTERVAL,
    enabled: opts?.enabled !== false,
  });
};
