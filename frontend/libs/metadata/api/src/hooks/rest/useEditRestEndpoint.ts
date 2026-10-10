import { hasuraToast } from '@hasura/shared/ui';
import { type RestEndpoint } from '@hasura/shared/types';
import {
  deleteAllowedQueryQuery,
  dropRESTEndpointQuery,
} from './useDropRestEndpoint';
import {
  MetadataMigrationOptions,
  useMetadata,
  useMetadataMigration,
} from '../metadata';
import { addAllowedQuery, createRestEndpointQuery } from './useAddRestEndpoint';
import { useErrorNotification } from '../notification';

type EditRestEndpointArgs = {
  oldEntry: RestEndpoint;
  newEntry: RestEndpoint;
  request: string;
};

export const useEditRestEndpoint = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { data: meta } = useMetadata();
  const showErrorNotification = useErrorNotification();

  const editRestEndpoint = async (
    { oldEntry, newEntry, request }: EditRestEndpointArgs,
    options?: MetadataMigrationOptions,
  ) => {
    const currentEndpoints = meta?.metadata?.rest_endpoints;
    if (!currentEndpoints?.length || !meta) {
      showErrorNotification({
        title: "Editing this REST endpoint isn't possible.",
        message: 'Your metadata seems to be empty!',
      });
      return;
    }

    const dropOldQueryFromCollection = deleteAllowedQueryQuery(oldEntry.name);
    const addNewQueryToCollection = addAllowedQuery({
      name: newEntry.name,
      query: request,
    });

    const args = [
      dropRESTEndpointQuery(oldEntry.name),
      dropOldQueryFromCollection,
      addNewQueryToCollection,
      createRestEndpointQuery(newEntry),
    ];

    return mutate(
      {
        query: {
          type: 'bulk',
          args,
          resource_version: meta.resource_version,
        },
      },
      {
        ...options,
        onSuccess: (data, variables, onMutateResult, context) => {
          hasuraToast({
            title: 'Successfully edited REST endpoint!',
            type: 'success',
          });
          options?.onSuccess?.(data, variables, onMutateResult, context);
        },
        onError: (err, variables, onMutateResult, context) => {
          showErrorNotification({
            title: 'Error editing REST endpoint!',
            error: err,
          });
          options?.onError?.(err, variables, onMutateResult, context);
        },
      },
    );
  };

  return {
    editRestEndpoint,
    ...rest,
  };
};
