import { hasuraToast } from '@hasura/shared/ui';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import { MetadataMigrationOptions, useMetadataMigration } from '../metadata';
import { QualifiedDataSource } from '@hasura/shared/types';
import { useErrorNotification } from '../notification';

export const useDropSource = (options?: MetadataMigrationOptions) => {
  const showErrorNotification = useErrorNotification();
  const { mutate, ...rest } = useMetadataMigration({
    onSuccess: (data, variables, onMutateResult, context) => {
      hasuraToast({ type: 'success', title: 'Source dropped from metadata!' });
      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
    onError: (error, variables, onMutateResult, context) => {
      showErrorNotification({
        title: 'Failed to drop source.',
        error,
      });
      options?.onError?.(error, variables, onMutateResult, context);
    },
  });

  const dropSource = (
    {
      source,
    }: {
      source: QualifiedDataSource;
    },
    mutationOptions?: MetadataMigrationOptions,
  ) => {
    return mutate(
      {
        query: {
          type: `${getDriverPrefix(source.kind)}_drop_source`,
          args: {
            name: source.name,
            cascade: true,
          },
        },
      },
      mutationOptions,
    );
  };

  return { dropSource, ...rest };
};
