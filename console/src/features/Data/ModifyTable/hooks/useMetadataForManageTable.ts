import { exportMetadata } from '@/features/DataSource';
import { Source } from '@/features/MetadataAPI';
import { useHttpClient } from '@/features/Network';
import { AxiosError } from 'axios';
import { useQuery } from 'react-query';

// adding underscore to prevent conflicts or confusion as this is only mean to be for this scope
export const useMetadataForManageTable = (dataSourceName: string) => {
  const httpClient = useHttpClient();

  return useQuery<Source | undefined, AxiosError>({
    queryKey: ['manage-table', dataSourceName],
    queryFn: async () => {
      const { metadata } = await exportMetadata({ httpClient });

      if (!metadata) throw Error('Unable to fetch sources from metadata');

      const metadataSource = metadata.sources.find(
        source => source.name === dataSourceName
      );

      if (!metadataSource) throw Error('Unable to find the source in metadata');

      return metadataSource;
    },

    refetchOnWindowFocus: false,
  });
};
