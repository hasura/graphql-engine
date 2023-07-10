import { useCallback } from 'react';
import { UPDATE_CURRENT_DATA_SOURCE } from '../../../components/Services/Data/DataActions';
import { hasuraToast } from '../../../new-components/Toasts';
import { useAppDispatch } from '../../../storeHooks';
import { useMetadataMigration } from '../../MetadataAPI';
import { MetadataMigrationOptions } from '../../MetadataAPI/hooks/useMetadataMigration';

export const useDropSource = (props?: MetadataMigrationOptions) => {
  const { ...globalMutateOptions } = props;

  const dispatch = useAppDispatch();

  const { mutate, ...rest } = useMetadataMigration({
    onSuccess: (data, variables, ctx) => {
      hasuraToast({ type: 'success', title: 'Source dropped from metadata!' });
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
