import { useCallback } from 'react';
import { useMetadataMigration, useMetadata } from '../metadata';
import { QueryCollectionQuery } from '@hasura/shared/types';
import { TMigrationSingleQuery } from '../../api';
import { createAllowedQueriesIfNeeded } from './useCreateQueryCollection';

export const useMoveOperationsToQueryCollection = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { data: metadata } = useMetadata();

  const moveOperationToQueryCollection = useCallback(
    (
      fromCollection: string,
      toCollection: string,
      queries: QueryCollectionQuery[],
      options?: Parameters<typeof mutate>[1],
    ) => {
      if (!fromCollection || !queries || !toCollection)
        throw Error(
          `useMoveOperationsToQueryCollection: Invalid input - ${
            !fromCollection && 'fromCollection'
          } ${!queries && ', queries'} ${!toCollection && ', toCollection'}`,
        );

      const restEndpoints = metadata?.metadata?.rest_endpoints || [];
      const args: TMigrationSingleQuery[] = [];

      queries.forEach((query) => {
        const isRestEndpoint = restEndpoints.some((e) => e.name === query.name);
        const restEndpoint = restEndpoints?.filter(
          (e) => e.name === query.name,
        );

        if (isRestEndpoint) {
          // Drop the rest endpoint first
          args.push({
            type: 'drop_rest_endpoint' as const,
            args: {
              name: query.name,
            },
          });
        }

        // Drop the query from the source collection
        args.push({
          type: 'drop_query_from_collection' as const,
          args: {
            collection_name: fromCollection,
            query_name: query.name,
          },
        });

        // Add the query to the target collection
        args.push({
          type: 'add_query_to_collection' as const,
          args: {
            collection_name: toCollection,
            query_name: query.name,
            query: query.query,
          },
        });

        if (isRestEndpoint) {
          restEndpoint[0].definition.query.collection_name = toCollection;
          // Re-add the restified endpoint
          args.push({
            type: 'create_rest_endpoint' as const,
            args: restEndpoint[0],
          });
        }
      });

      return mutate(
        {
          query: {
            type: 'bulk',
            resource_version: metadata?.resource_version,
            args: [
              ...createAllowedQueriesIfNeeded(toCollection, metadata),
              ...args,
            ],
          },
        },
        options,
      );
    },
    [metadata, mutate],
  );

  return { moveOperationToQueryCollection, ...rest };
};
