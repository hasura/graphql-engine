import React from 'react';
import { useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { DriverInfo, getAllSourceKinds } from '../../driver';

// default options should return pretty much `list_source_kinds` response
export const useAvailableDrivers = ({
  onFirstSuccess,
}: {
  onFirstSuccess?: (drivers: DriverInfo[]) => void;
} = {}) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const firstSuccess = React.useRef(true);

  const query = useQuery({
    queryKey: ['get_available_drivers'],
    queryFn: async (): Promise<DriverInfo[]> => {
      const unfilteredDrivers = await getAllSourceKinds({
        endpoints,
        fetchJson,
      });
      return unfilteredDrivers.filter(
        (driver) => driver.release !== 'disabled',
      );
    },
    refetchOnWindowFocus: false,
  });

  React.useEffect(() => {
    if (query.isSuccess && firstSuccess.current === true) {
      onFirstSuccess?.(query.data);
      firstSuccess.current = false;
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [query.isSuccess, query.data]);

  return query;
};
