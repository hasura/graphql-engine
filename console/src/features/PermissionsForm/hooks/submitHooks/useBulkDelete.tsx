import {
  useMetadataTablePermissions,
  useMetadataMigration,
  useMetadataVersion,
} from '@/features/MetadataAPI';
import { useAppSelector } from '@/store';
import { currentDriver } from '../../../../dataSources';

import { api } from '../../api';
import { QueryType } from '../../types';

export interface UseDeletePermissionArgs {
  tableName: string;
  schemaName: string;
}

const useDataTarget = ({ schemaName, tableName }: UseDeletePermissionArgs) => {
  const dataSource: string = useAppSelector(
    state => state.tables.currentDataSource || 'default'
  );

  const driver = currentDriver;

  const {
    data: resourceVersion,
    isLoading: metadataLoading,
    isError: metadataError,
  } = useMetadataVersion();

  if (!resourceVersion && !metadataLoading) {
    throw new Error('No resource version');
  }

  const {
    data: permissions,
    isLoading: permissionsLoading,
    isError: permissionsError,
  } = useMetadataTablePermissions(
    {
      schema: schemaName,
      name: tableName,
    },
    dataSource
  );

  const isLoading = permissionsLoading || metadataLoading;
  const isError = permissionsError || metadataError;

  return {
    driver,
    dataSource,
    resourceVersion,
    permissions,
    isLoading,
    isError,
  };
};

interface RoleList {
  roleName: string;
  queries: QueryType[];
}

export const useBulkDeletePermissions = ({
  tableName,
  schemaName,
}: UseDeletePermissionArgs) => {
  const {
    driver,
    resourceVersion,
    permissions,
    dataSource,
    isLoading: dataTargetLoading,
    isError: dataTargetError,
  } = useDataTarget({ schemaName, tableName });

  const {
    mutateAsync,
    isLoading: mutationLoading,
    isError: mutationError,
    ...rest
  } = useMetadataMigration();

  const submit = async (roles: string[]) => {
    if (!resourceVersion) {
      console.error('No resource version');
      return;
    }

    const roleList = permissions?.reduce<RoleList[]>((acc, each) => {
      if (roles.includes(each.role_name)) {
        acc.push({
          roleName: each.role_name,
          queries: Object.keys(each.permissions) as QueryType[],
        });
      }

      return acc;
    }, []);

    const body = api.createBulkDeleteBody({
      driver,
      dataTarget: {
        database: dataSource,
        table: tableName,
        schema: schemaName,
      },
      roleName: '',
      resourceVersion,
      roleList,
    });

    await mutateAsync({
      source: dataSource,
      query: body,
      migrationName: 'bulkDeletePermissions',
    });
  };

  const isLoading = dataTargetLoading || mutationLoading;
  const isError = dataTargetError || mutationError;

  return {
    submit,
    ...rest,
    isError,
    isLoading,
  };
};
