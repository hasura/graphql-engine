import { useInvalidateMetadata } from '../../hasura-metadata-api';
import { useMetadataMigration } from '../../MetadataAPI';
import { hasuraToast } from '../../../new-components/Toasts';
import { useCallback } from 'react';
import { MetadataMigrationOptions } from '../../MetadataAPI/hooks/useMetadataMigration';
import { useAppDispatch } from '../../../storeHooks';
import { UPDATE_CURRENT_DATA_SOURCE } from '../../../components/Services/Data/DataActions';

export const useDropSource = (props?: MetadataMigrationOptions) => {
  const { ...globalMutateOptions } = props;
  const dispatch = useAppDispatch();
  const invalidateMetadata = useInvalidateMetadata();
  const { mutate, ...rest } = useMetadataMigration({
    onSuccess: (data, variables, ctx) => {
      hasuraToast({ type: 'success', title: 'Source dropped from metadata!' });
      invalidateMetadata();
      dispatch({
        type: UPDATE_CURRENT_DATA_SOURCE,
        source: '',
      });
      globalMutateOptions?.onSuccess?.(data, variables, ctx);
    },
    onError: (data, variables, ctx) => {
      hasuraToast({ type: 'error', title: 'Failed to drop source.' });
      globalMutateOptions?.onError?.(data, variables, ctx);
    },
  });

  const dropSource = useCallback(
    async ({
      driver,
      dataSourceName,
      ...mutationOptions
    }: {
      driver: string;
      dataSourceName: string;
    } & MetadataMigrationOptions) => {
      mutate(
        {
          query: {
            type: `${driver}_drop_source`,
            args: {
              name: dataSourceName,
              cascade: true,
            },
          },
        },
        mutationOptions
      );
    },
    [mutate]
  );
  return { dropSource, ...rest };
};
