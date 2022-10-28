import { useQueryClient } from 'react-query';
import { AxiosInstance } from 'axios';

import {
  useMetadataMigration,
  useMetadataVersion,
} from '@/features/MetadataAPI';
import { exportMetadata } from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';

import { AccessType, FormOutput, QueryType } from '../../types';
import { api } from '../../api';

export interface UseSubmitFormArgs {
  currentSource: string;
  dataSourceName: string;
  table: unknown;
  roleName: string;
  queryType: QueryType;
  accessType: AccessType;
}

const metadataPermissionKeys = [
  'insert_permissions',
  'select_permissions',
  'update_permissions',
  'delete_permissions',
] as const;

export const keyToPermission = {
  insert_permissions: 'insert',
  select_permissions: 'select',
  update_permissions: 'update',
  delete_permissions: 'delete',
} as const;

const isPermission = (props: {
  key: string;
  value: any;
}): props is {
  key: typeof metadataPermissionKeys[number];
  // value: Permission[];
  value: any[];
} => props.key in keyToPermission;

interface ExistingPermissions {
  role: string;
  queryType: QueryType;
  table: unknown;
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
  const {
    currentSource,
    dataSourceName,
    table,
    roleName,
    queryType,
    accessType,
  } = args;
  const {
    data: resourceVersion,
    isLoading: resourceVersionLoading,
    isError: resourceVersionError,
  } = useMetadataVersion();

  const queryClient = useQueryClient();
  const httpClient = useHttpClient();

  const mutate = useMetadataMigration();

  const submit = async (formData: FormOutput) => {
    if (!resourceVersion) {
      console.error('No resource version');
      return;
    }

    const existingPermissions = await getAllPermissions({
      dataSourceName,
      httpClient,
    });

    const body = api.createInsertBody({
      currentSource,
      dataSourceName,
      table,
      roleName,
      queryType,
      accessType,
      resourceVersion,
      formData,
      existingPermissions,
    });

    await mutate.mutateAsync({
      query: body,
    });

    await queryClient.invalidateQueries([
      dataSourceName,
      'permissionDefaultValues',
      roleName,
      queryType,
    ]);
    await queryClient.invalidateQueries([dataSourceName, 'permissionsTable']);
  };

  const isLoading = mutate.isLoading || resourceVersionLoading;
  const isError = mutate.isError || resourceVersionError;

  return {
    submit,
    ...mutate,
    isLoading,
    isError,
  };
};
