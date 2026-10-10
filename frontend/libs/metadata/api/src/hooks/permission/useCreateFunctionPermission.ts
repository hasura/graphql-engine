import {
  MetadataMigrationOptions,
  useMetadataMigration,
} from '../metadata/useMetadataMigration';
import { getDriverPrefix, MetadataSelectors } from '@hasura/metadata/helpers';
import { useMetadataHelpers } from '../metadata';
import { TableFunction } from '@hasura/shared/types';
import { hasuraToast } from '@hasura/shared/ui';
import { useErrorNotification } from '../notification';

export const useCreateFunctionPermission = (
  globalOptions?: MetadataMigrationOptions,
) => {
  const { mutate, ...rest } = useMetadataMigration();
  const { fetchMetadata } = useMetadataHelpers();
  const showErrorNotification = useErrorNotification();

  const createFunctionPermission = async (
    {
      dataSourceName,
      qualifiedFunction,
      role,
    }: {
      dataSourceName: string;
      qualifiedFunction: TableFunction;
      role: string;
    },
    mutationOptions?: MetadataMigrationOptions,
  ) => {
    const meta = await fetchMetadata();
    const source = MetadataSelectors.findSource(dataSourceName)(meta);
    if (!source) {
      throw new Error(`Source ${source} does not exist or was dropped`);
    }

    return mutate(
      {
        query: {
          type: `${getDriverPrefix(source.kind)}_create_function_permission`,
          args: {
            function: qualifiedFunction,
            role,
            source: dataSourceName,
          },
          resource_version: meta.resource_version,
        },
      },
      {
        ...globalOptions,
        ...mutationOptions,
        onSuccess: (data, variables, onMutateResult, context) => {
          hasuraToast({
            type: 'success',
            title: 'Success!',
            message: 'Permission saved successfully!',
          });

          globalOptions?.onSuccess?.(data, variables, onMutateResult, context);
          mutationOptions?.onSuccess?.(
            data,
            variables,
            onMutateResult,
            context,
          );
        },
        onError: (err, variables, onMutateResult, context) => {
          showErrorNotification({
            title: 'Something went wrong while saving permission',
            error: err,
          });
          globalOptions?.onError?.(err, variables, onMutateResult, context);
          mutationOptions?.onError?.(err, variables, onMutateResult, context);
        },
      },
    );
  };

  return { createFunctionPermission, ...rest };
};
