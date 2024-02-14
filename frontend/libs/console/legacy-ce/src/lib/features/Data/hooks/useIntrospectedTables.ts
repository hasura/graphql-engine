import { UseQueryOptions, useQuery } from 'react-query';
import { DataSource, Feature, IntrospectedTable } from '../../DataSource';
import { useHttpClient } from '../../Network';
import { APIError } from '../../../hooks/error';
import { MetadataSelectors, useMetadata } from '../../hasura-metadata-api';

export const useIntrospectedTables = <
  FinalResult = Feature | IntrospectedTable[]
>({
  dataSourceName,
  select,
  options = {},
}: {
  dataSourceName: string;
  select?: (data: Feature | IntrospectedTable[]) => FinalResult;
  options?: UseQueryOptions<
    Feature | IntrospectedTable[],
    APIError,
    FinalResult
  >;
}) => {
  const httpClient = useHttpClient();

  const { data: source, isFetching: isFetchingMetadata } = useMetadata(
    MetadataSelectors.findSource(dataSourceName)
  );

  const isMongo = source?.kind === 'mongo';

  return useQuery<Feature | IntrospectedTable[], APIError, FinalResult>(
    [dataSourceName, 'untracked_tables'],
    async () => {
      const result = await DataSource(httpClient).introspectTables({
        dataSourceName,
      });

      if (isMongo && Array.isArray(result)) {
        return result.map(collection => ({
          ...collection,
          type: 'COLLECTION',
        }));
      }

      return result;
    },
    {
      select,
      enabled: !isFetchingMetadata,
      ...options,
    }
  );
};
