import { defaultQueryOptions, useMetadata } from '@hasura/metadata/api';
import type { TreeDataNode } from '@hasura/shared/ui';
import { useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { isNativeDriver } from '@hasura/metadata/helpers';
import { useAppContext } from '@hasura/shared/context';
import { useAvailableDrivers } from './useAvailableDrivers';
import { getDatabaseMethods } from '../../driver';

const isValueDataNode = (value: TreeDataNode | null): value is TreeDataNode =>
  value !== null;

const GET_TREE_DATA_QUERY_KEY = 'GET_TREE_DATA';

export const useTreeData = () => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const { data: metadata, isFetching } = useMetadata();
  const { data: availableDrivers } = useAvailableDrivers();

  return useQuery({
    queryKey: [GET_TREE_DATA_QUERY_KEY],
    queryFn: async () => {
      if (!metadata) throw new Error('Unable to fetch metadata');

      const treeData = metadata.metadata.sources
        /**
         * NOTE: this filter prevents native drivers from being part of the new tree
         */
        .filter((source) => !isNativeDriver(source.kind))
        .map(async (source) => {
          const releaseName = availableDrivers?.find(
            (driver) => driver.name === source.kind,
          )?.release;

          const tablesAsTree = getDatabaseMethods(
            source.kind,
          ).introspection.getTablesListAsTree({
            endpoints,
            fetchJson,
            dataSourceName: source.name,
            releaseName,
          });
          return tablesAsTree;
        });

      const promisesResult = await Promise.all(treeData);

      const filteredResult =
        promisesResult.filter<TreeDataNode>(isValueDataNode);

      return filteredResult;
    },
    enabled: !isFetching && !!availableDrivers,
    ...defaultQueryOptions,
  });
};
