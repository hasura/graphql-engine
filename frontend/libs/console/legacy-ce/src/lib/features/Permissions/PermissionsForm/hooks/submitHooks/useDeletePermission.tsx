import { useQueryClient } from 'react-query';
import { useMetadataMigration } from '../../../../MetadataAPI';
import { exportMetadata } from '../../../../DataSource';
import { useHttpClient } from '../../../../Network';
import { QueryType } from '../../../types';
import { api } from '../../api';
import { permissionsTableKey } from '../../../PermissionsTable/hooks';
import { permissionsFormKey } from '../dataFetchingHooks';
import { transformErrorResponse } from '../../../../Data/errorUtils';
import { hasuraToast } from '../../../../../new-components/Toasts';
import { DisplayToastErrorMessage } from '../../../../Data/components/DisplayErrorMessage';

export interface UseDeletePermissionArgs {
  dataSourceName: string;
  table: unknown;
  roleName: string;
}

export const useDeletePermission = ({
  dataSourceName,
  table,
  roleName,
}: UseDeletePermissionArgs) => {
  const mutate = useMetadataMigration({
    errorTransform: transformErrorResponse,
  });
  const httpClient = useHttpClient();
  const queryClient = useQueryClient();

  const submit = async (queries: QueryType[]) => {
    const { resource_version: resourceVersion, metadata } =
      await exportMetadata({
        httpClient,
      });

    if (!resourceVersion) {
      console.error('No resource version');
      return;
    }

    const driver = metadata.sources.find(s => s.name === dataSourceName)?.kind;

    if (!driver) throw Error('Unable to find driver in metadata');

    const body = api.createDeleteBody({
      driver,
      dataSourceName,
      table,
      role: roleName,
      resourceVersion,
      queries,
    });

    await mutate.mutateAsync(
      {
        query: body,
      },
      {
        onSuccess: () => {
          hasuraToast({
            type: 'success',
            title: 'Success!',
            message: 'Permissions successfully deleted',
          });
        },
        onError: err => {
          hasuraToast({
            type: 'error',
            title: 'Error!',
            children: <DisplayToastErrorMessage message={err.message} />,
          });
        },
        onSettled: async () => {
          await queryClient.invalidateQueries(
            permissionsFormKey({
              dataSourceName,
              table,
            })
          );

          await queryClient.invalidateQueries(
            permissionsTableKey({
              dataSourceName,
              table,
            })
          );
        },
      }
    );
  };

  const isLoading = mutate.isLoading;
  const isError = mutate.isError;

  return {
    submit,
    ...mutate,
    isLoading,
    isError,
  };
};
