import { useQuery } from '@tanstack/react-query';
import { useTableDefinition } from '../../Data';
import { useMetadata } from '@hasura/metadata/api';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { getConnectDatabaseFormSchema } from '@hasura/metadata/data-source';

export const useEditDataSourceConnectionInfo = () => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const { data: meta, isLoading } = useMetadata();

  const urlData = useTableDefinition();

  return useQuery({
    queryKey: ['edit-connection'],
    queryFn: async () => {
      if (urlData.querystringParseResult === 'error')
        throw Error('Something went wrong while parsing the URL parameters');
      const { database: dataSourceName } = urlData.data;

      const metadataSource = meta?.metadata.sources.find(
        (source) => source.name === dataSourceName,
      );

      if (!metadataSource) throw Error('Unavailable to fetch metadata source');

      const schema = await getConnectDatabaseFormSchema({
        driver: metadataSource.kind,
        endpoints,
        fetchJson,
      });

      return {
        schema,
        configuration: metadataSource.configuration,
        driver: metadataSource.kind,
        name: metadataSource.name,
        customization: metadataSource.customization,
      };
    },
    enabled: !isLoading,
  });
};
