import { hasuraToast } from '@hasura/shared/ui';
import { getConfirmation } from '@hasura/shared/utils';
import { allowedQueriesCollection } from '@hasura/shared/types';
import {
  MetadataMigrationOptions,
  useMetadata,
  useMetadataMigration,
} from '../metadata';
import { useErrorNotification } from '../notification';

export const dropRESTEndpointQuery = (name: string) => ({
  type: 'drop_rest_endpoint' as const,
  args: { name },
});

export const deleteAllowedQueryQuery = (
  queryName: string,
  collectionName = allowedQueriesCollection,
) => ({
  type: 'drop_query_from_collection' as const,
  args: {
    collection_name: collectionName,
    query_name: queryName,
  },
});

type PropsRESTEndpointDrops = {
  name: string;
  request: string;
};

type UseDropRESTEndpointOptions = MetadataMigrationOptions;

export const useDropRestEndpoint = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { data: meta } = useMetadata();
  const showErrorNotification = useErrorNotification();

  const dropRestEndpoint = async (
    { name, request }: PropsRESTEndpointDrops,
    options?: UseDropRESTEndpointOptions,
  ) => {
    const currentRESTEndpoints = meta?.metadata?.rest_endpoints;

    if (!currentRESTEndpoints?.length || !meta) {
      hasuraToast({
        type: 'error',
        title: "Deletion of REST endpoint isn't possible.",
        message: 'Your metadata seems to be empty!',
      });
      return;
    }

    const currentObj = currentRESTEndpoints.find((re) => re.name === name);

    if (!currentObj) {
      showErrorNotification({
        title: "Deletion of REST endpoint isn't possible.",
        message: `We could not find the endpoint ${name} to delete`,
      });
      return;
    }

    const confirmation = getConfirmation(
      `You want to delete the endpoint: ${name}`,
    );

    if (!confirmation) {
      return;
    }

    return mutate(
      {
        query: {
          type: 'bulk',
          args: [dropRESTEndpointQuery(name), deleteAllowedQueryQuery(request)],
        },
      },
      {
        ...options,
        onSuccess: (data, variables, onMutateResult, context) => {
          hasuraToast({
            title: 'Successfully dropped REST endpoint!',
            type: 'success',
          });

          options?.onSuccess?.(data, variables, onMutateResult, context);
        },
        onError: (err, variables, onMutateResult, context) => {
          showErrorNotification({
            title: 'Error dropping REST endpoint!',
            error: err,
          });

          options?.onError?.(err, variables, onMutateResult, context);
        },
      },
    );
  };

  return {
    ...rest,
    dropRestEndpoint,
  };
};
