import type { QueryCollectionQuery } from '@hasura/shared/types';
import { useMetadataHelpers, useMetadataMigrationMutation } from '../metadata';
import { TMigrationSingleQuery } from '../../api';
import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';

export const removeOperationsFromQueryCollectionMetadataArgs = (
  queryCollection: string,
  queries: string[],
) => [
  ...queries.map((query) => ({
    type: 'drop_query_from_collection' as const,
    args: {
      collection_name: queryCollection,
      query_name: query,
    },
  })),
];

export const useRemoveOperationsFromQueryCollection = () => {
  const { fetchMetadata } = useMetadataHelpers();

  return useMetadataMigrationMutation(
    async ({
      queryCollection,
      queries,
    }: {
      queryCollection: string;
      queries: QueryCollectionQuery[];
    }) => {
      if (!queryCollection || !queries) {
        throw Error(
          `useRemoveOperationsFromQueryCollection: Invalid input - ${
            queryCollection && 'queryCollection'
          } ${queries && 'queries'}`,
        );
      }

      const meta = await fetchMetadata();
      const restEndpoints = meta.metadata.rest_endpoints || [];
      const args: TMigrationSingleQuery[] = [];

      queries.forEach((endpoint) => {
        if (
          restEndpoints.some(
            (e) =>
              e.name === endpoint.name &&
              e.definition.query.collection_name === queryCollection,
          )
        ) {
          args.push({
            type: 'drop_rest_endpoint',
            args: {
              name: endpoint.name,
            },
          });
        }
      });

      args.push(
        ...removeOperationsFromQueryCollectionMetadataArgs(
          queryCollection,
          queries.map((query) => query.name),
        ),
      );

      return {
        type: 'bulk',
        resource_version: meta.resource_version,
        args,
      };
    },
    {
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: 'Operation deleted',
          message: 'Operation deleted successfully!',
        });
      },
      onError: (error) => {
        showErrorNotification({
          title: 'Failed to delete operation',
          error,
        });
      },
    },
  );
};
