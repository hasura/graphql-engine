import { DEFAULT_STALE_TIME } from '@/features/DatabaseRelationships';
import { DataSource, nativeDrivers } from '@/features/DataSource';
import { useMetadata } from '@/features/hasura-metadata-api';
import { useHttpClient } from '@/features/Network';
import { DataNode } from 'antd/lib/tree';
import { useQuery } from 'react-query';

const isValueDataNode = (value: DataNode | null): value is DataNode =>
  value !== null;

export const useTreeData = () => {
  const httpClient = useHttpClient();
  const { data: metadata, isFetching } = useMetadata();

  return useQuery({
    queryKey: ['treeview'],
    queryFn: async () => {
      if (!metadata) throw Error('Unable to fetch metadata');

      const treeData = metadata.metadata.sources
        /**
         * NOTE: this filter prevents native drivers from being part of the new tree
         */
        .filter(source => !nativeDrivers.includes(source.kind))
        .map(async source => {
          const tablesAsTree = await DataSource(
            httpClient
          ).getTablesWithHierarchy({ dataSourceName: source.name });
          return tablesAsTree;
        });

      const promisesResult = await Promise.all(treeData);

      const filteredResult = promisesResult.filter<DataNode>(isValueDataNode);

      return filteredResult;
    },
    enabled: !isFetching,
    refetchOnWindowFocus: false,
    staleTime: DEFAULT_STALE_TIME,
  });
};
