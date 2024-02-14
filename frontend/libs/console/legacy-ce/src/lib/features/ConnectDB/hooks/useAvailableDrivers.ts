import React from 'react';
import { useQuery } from 'react-query';
import { DataSource, DriverInfo } from '../../DataSource';
import { useHttpClient } from '../../Network';

// default options should return pretty much `list_source_kinds` response
export const useAvailableDrivers = ({
  onFirstSuccess,
}: {
  onFirstSuccess?: (drivers: DriverInfo[]) => void;
} = {}) => {
  const httpClient = useHttpClient();

  const firstSuccess = React.useRef(true);

  return useQuery({
    queryKey: ['get_available_drivers'],
    queryFn: async () => {
      const unfilteredDrivers = await DataSource(
        httpClient
      ).driver.getAllSourceKinds();
      return unfilteredDrivers.filter(driver => driver.release !== 'disabled');
    },
    onSuccess: drivers => {
      if (firstSuccess.current === true) {
        onFirstSuccess?.(drivers);
        firstSuccess.current = false;
      }
    },
    refetchOnWindowFocus: false,
  });
};
