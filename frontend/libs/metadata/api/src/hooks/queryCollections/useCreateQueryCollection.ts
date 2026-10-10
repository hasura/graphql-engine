import { TMigrationSingleQuery } from '../../api';
import {
  MetadataMigrationOptions,
  useMetadata,
  useMetadataMigration,
} from '../metadata';
import { Metadata } from '@hasura/shared/types';

type CreateQueryCollectionProps = { name: string; addToAllowList?: boolean };

export const useCreateQueryCollection = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { data: metadata } = useMetadata();

  const createQueryCollection = async (
    { name, addToAllowList }: CreateQueryCollectionProps,
    mutateOptions?: MetadataMigrationOptions,
  ) => {
    return mutate(
      {
        query: {
          resource_version: metadata?.resource_version,
          type: 'bulk',
          args: [
            {
              type: 'create_query_collection' as const,
              args: {
                name,
                definition: {
                  queries: [],
                },
              },
            },
            addToAllowList
              ? {
                  type: 'add_collection_to_allowlist' as const,
                  args: {
                    collection: name,
                  },
                }
              : undefined,
          ].filter((op) => op) as TMigrationSingleQuery[],
        },
      },
      mutateOptions,
    );
  };

  return { createQueryCollection, ...rest };
};

export const createAllowedQueriesIfNeeded = (
  queryCollection: string,
  metadata: Metadata | undefined,
) => {
  return queryCollection === 'allowed-queries' &&
    !metadata?.metadata?.query_collections?.find(
      (q) => q.name === queryCollection,
    )
    ? [
        {
          type: 'create_query_collection' as const,
          args: {
            name: queryCollection,
            definition: {
              queries: [],
            },
          },
        },
        {
          type: 'add_collection_to_allowlist' as const,
          args: {
            collection: queryCollection,
          },
        },
      ]
    : [];
};
