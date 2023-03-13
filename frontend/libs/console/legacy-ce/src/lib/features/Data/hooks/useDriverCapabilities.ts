import { useQuery } from 'react-query';
import { DataSource } from '../../DataSource';
import { useMetadata } from '../../hasura-metadata-api';
import { useHttpClient } from '../../Network';

type UseDatabaseCapabilitiesArgs = {
  dataSourceName: string;
};

export const useDriverCapabilities = ({
  dataSourceName,
}: UseDatabaseCapabilitiesArgs) => {
  const httpClient = useHttpClient();

  const { data: driverName, isFetching: isFetchingMetadata } = useMetadata(
    m => m.metadata.sources.find(source => source.name === dataSourceName)?.kind
  );

  const result = useQuery({
    queryKey: [driverName, 'capabilities'],
    queryFn: async () => {
      if (!driverName) {
        throw Error('Unable to retrieve driver name from metadata');
      }

      return DataSource(httpClient).getDriverCapabilities(driverName);
    },
    enabled: !isFetchingMetadata,
  });

  return result;
};
