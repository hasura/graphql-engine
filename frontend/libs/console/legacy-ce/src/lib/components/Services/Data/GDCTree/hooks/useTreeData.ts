import {
  DataSource,
  exportMetadata,
  nativeDrivers,
} from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';
import { DataNode } from 'antd/lib/tree';
import { useQuery } from 'react-query';

const isValueDataNode = (value: DataNode | null): value is DataNode =>
  value !== null;

export const useTreeData = () => {
  const httpClient = useHttpClient();

  return useQuery({
    queryKey: ['treeview'],
    queryFn: async () => {
      const { metadata } = await exportMetadata({ httpClient });

      if (!metadata) throw Error('Unable to fetch metadata');

      const treeData = metadata.sources
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
  });
};
