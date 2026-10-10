import { useQueryClient } from '@tanstack/react-query';
import { api } from '../../api';
import { permissionsTableKey } from '../../../PermissionsTable/hooks';
import { DisplayToastErrorMessage, hasuraToast } from '@hasura/shared/ui';
import { useMetadataHelpers, useMetadataMigration } from '@hasura/metadata/api';
import type { DataQueryType, Table } from '@hasura/shared/types';

export interface UseDeletePermissionArgs {
  dataSourceName: string;
  table: Table;
  roleName: string;
}

export const useDeletePermission = ({
  dataSourceName,
  table,
  roleName,
}: UseDeletePermissionArgs) => {
  const mutate = useMetadataMigration();
  const queryClient = useQueryClient();
  const { fetchSource } = useMetadataHelpers();

  const submit = async (queries: DataQueryType[]) => {
    const { source, resource_version } = await fetchSource(dataSourceName);

    const driver = source.kind;

    const body = api.createDeleteBody({
      driver,
      dataSourceName,
      table,
      role: roleName,
      resourceVersion: resource_version,
      queries,
    });

    await mutate.mutate(
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
        onError: (err) => {
          hasuraToast({
            type: 'error',
            title: 'Error!',
            children: <DisplayToastErrorMessage message={err.message} />,
          });
        },
        onSettled: async () => {
          await queryClient.invalidateQueries({
            queryKey: permissionsTableKey({
              dataSourceName,
              table,
            }),
          });
        },
      },
    );
  };

  const isLoading = mutate.isPending;
  const isError = mutate.isError;

  return {
    submit,
    ...mutate,
    isLoading,
    isError,
  };
};
