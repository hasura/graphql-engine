import { exportMetadata } from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';
import { useQuery } from 'react-query';

export const useMetadataSource = (dataSourceName: string) => {
  const httpClient = useHttpClient();
  return useQuery(
    ['export_metadata', 'trackTables', 'metadataSource'],
    async () => {
      const result = await exportMetadata({ httpClient });

      if (!result) throw Error('useMetadataSource: cannot export metadata');

      const driver = result.metadata.sources.find(
        source => source.name === dataSourceName
      )?.kind;

      if (!driver)
        throw Error('useMetadataSource: cannot find source in metadata');

      return { metadata: result, driver };
    }
  );
};
