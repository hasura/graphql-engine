import { DataSource } from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';
import { useQuery } from 'react-query';

export const useAvailableDrivers = () => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: ['get_available_drivers'],
    queryFn: async () => {
      const drivers = await DataSource(httpClient).driver.getAllSourceKinds();
      return drivers;
    },
  });
};
