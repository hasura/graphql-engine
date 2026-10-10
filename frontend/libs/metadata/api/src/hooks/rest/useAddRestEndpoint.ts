import { hasuraToast } from '@hasura/shared/ui';
import {
  MetadataMigrationOptions,
  useMetadata,
  useMetadataMigration,
} from '../metadata';
import {
  allowedQueriesCollection,
  type RestEndpoint,
} from '@hasura/shared/types';
import { TMigrationSingleQuery } from '../../api';
import { useErrorNotification } from '../notification';

export const addAllowedQuery = (
  query: { name: string; query: string },
  collectionName = allowedQueriesCollection,
) => ({
  type: 'add_query_to_collection' as const,
  args: {
    collection_name: collectionName,
    query_name: query.name,
    query: query.query,
  },
});

export const createAllowListQuery = (
  queries: Array<{ name: string; query: string }>,
  source?: string,
) => {
  const createAllowListCollectionQuery = {
    type: 'create_query_collection' as const,
    args: {
      name: allowedQueriesCollection,
      definition: {
        queries,
      },
    },
  };

  const addCollectionToAllowListQuery = {
    type: 'add_collection_to_allowlist' as const,
    args: {
      collection: allowedQueriesCollection,
    },
  };

  return {
    type: 'bulk',
    source,
    args: [createAllowListCollectionQuery, addCollectionToAllowListQuery],
  };
};

export const createRestEndpointQuery = (args: RestEndpoint) => ({
  type: 'create_rest_endpoint' as const,
  args,
});

export type AddRestEndpointProps = {
  entry: RestEndpoint;
  request: string;
};

export const useAddRestEndpoint = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { data: meta } = useMetadata();
  const showErrorNotification = useErrorNotification();

  const addRestEndpoint = async (
    { entry, request }: AddRestEndpointProps,
    options?: MetadataMigrationOptions,
  ) => {
    const consoleCollection = meta?.metadata?.query_collections?.find(
      (collection) => collection.name === allowedQueriesCollection,
    );

    const args: TMigrationSingleQuery[] = consoleCollection
      ? [addAllowedQuery({ name: entry.name, query: request })]
      : createAllowListQuery([{ name: entry.name, query: request }]).args;

    // the REST endpoint based requests
    args.push(createRestEndpointQuery(entry));

    return mutate(
      {
        query: {
          type: 'bulk',
          args,
        },
      },
      {
        ...options,
        onSuccess: (data, variables, onMutateResult, context) => {
          hasuraToast({
            title: 'Successfully created REST endpoint!',
            type: 'success',
          });

          options?.onSuccess?.(data, variables, onMutateResult, context);
        },
        onError: (err, variables, onMutateResult, context) => {
          showErrorNotification({
            title: 'Error creating REST endpoint!',
            error: err,
          });
          options?.onError?.(err, variables, onMutateResult, context);
        },
      },
    );
  };

  return {
    ...rest,
    addRestEndpoint,
  };
};
