import { useQueryClient } from 'react-query';
import { AxiosInstance } from 'axios';

import { useMetadataMigration } from '../../../../MetadataAPI';
import { exportMetadata } from '../../../../DataSource';
import { useHttpClient } from '../../../../Network';
import { useFireNotification } from '../../../../../new-components/Notifications';
import { AccessType, QueryType } from '../../../types';
import { api } from '../../api';
import { isPermission, keyToPermission } from '../../../utils';
import { PermissionsSchema } from '../../../schema';
import { Table } from '../../../../hasura-metadata-types';

export interface UseSubmitFormArgs {
  dataSourceName: string;
  table: Table;
  tables: Table[];
  roleName: string;
  queryType: QueryType;
  accessType: AccessType;
}

interface ExistingPermissions {
  role: string;
  queryType: QueryType;
  table: Table;
}

interface GetAllPermissionsArgs {
  dataSourceName: string;
  httpClient: AxiosInstance;
}

const getAllPermissions = async ({
  dataSourceName,
  httpClient,
}: GetAllPermissionsArgs) => {
  const { metadata } = await exportMetadata({ httpClient });

  // find current source
  const currentMetadataSource = metadata?.sources?.find(
    source => source.name === dataSourceName
  );

  return currentMetadataSource?.tables.reduce<ExistingPermissions[]>(
    (acc, metadataTable) => {
      Object.entries(metadataTable).forEach(([key, value]) => {
        const props = { key, value };
        if (isPermission(props)) {
          props.value.forEach(permission => {
            acc.push({
              role: permission.role,
              queryType: keyToPermission[props.key],
              table: metadataTable.table,
            });
          });
        }
      });

      return acc;
    },
    []
  );
};

export const useSubmitForm = (args: UseSubmitFormArgs) => {
  const { dataSourceName, table, roleName, queryType, accessType, tables } =
    args;

  const queryClient = useQueryClient();
  const httpClient = useHttpClient();

  const { fireNotification } = useFireNotification();

  const mutate = useMetadataMigration();

  const submit = async (formData: PermissionsSchema) => {
    const { metadata, resource_version } = await exportMetadata({
      httpClient,
    });

    const metadataSource = metadata?.sources.find(
      s => s.name === dataSourceName
    );

    if (!resource_version || !metadataSource) {
      console.error('Something went wrong!');
      return;
    }

    const existingPermissions = await getAllPermissions({
      dataSourceName,
      httpClient,
    });

    const body = api.createInsertBody({
      dataSourceName,
      driver: metadataSource.kind,
      table,
      tables,
      role: roleName,
      queryType,
      accessType,
      resourceVersion: resource_version,
      formData,
      existingPermissions: existingPermissions ?? [],
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
            message: 'Permissions saved successfully!',
          });
          exportMetadata({
            httpClient,
          });
        },
        onError: err => {
          fireNotification({
            type: 'error',
            title: 'Error!',
            message:
              err?.message ?? 'Something went wrong while saving permissions',
          });
        },
        onSettled: () => {
          queryClient.invalidateQueries(['export_metadata', 'roles']);
          queryClient.invalidateQueries([
            dataSourceName,
            'permissionFormData',
            JSON.stringify(table),
            roleName,
          ]);
          queryClient.invalidateQueries([
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
