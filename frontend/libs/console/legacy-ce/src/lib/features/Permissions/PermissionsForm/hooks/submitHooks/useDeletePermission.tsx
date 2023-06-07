import { useQueryClient } from 'react-query';
import { useMetadataMigration } from '../../../../MetadataAPI';
import { exportMetadata } from '../../../../DataSource';
import { useFireNotification } from '../../../../../new-components/Notifications';

import { useHttpClient } from '../../../../Network';

import { QueryType } from '../../../types';
import { api } from '../../api';

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
  const mutate = useMetadataMigration();
  const httpClient = useHttpClient();
  const queryClient = useQueryClient();
  const { fireNotification } = useFireNotification();

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
          fireNotification({
            type: 'success',
            title: 'Success!',
            message: 'Permissions successfully deleted',
          });
        },
        onError: err => {
          fireNotification({
            type: 'error',
            title: 'Error!',
            message:
              err?.message ?? 'Something went wrong while deleting permissions',
          });
        },
        onSettled: async () => {
          await queryClient.invalidateQueries([
            dataSourceName,
            'permissionFormData',
            JSON.stringify(table),
          ]);

          await queryClient.invalidateQueries([
            dataSourceName,
            'permissionsTable',
            JSON.stringify(table),
          ]);
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
