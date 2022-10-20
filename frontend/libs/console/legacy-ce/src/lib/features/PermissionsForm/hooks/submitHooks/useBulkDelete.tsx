import {
  useMetadataTablePermissions,
  useMetadataMigration,
  useMetadataVersion,
} from '@/features/MetadataAPI';

import { NewDataTarget } from '../../../PermissionsTab/types/types';

import { api } from '../../api';
import { QueryType } from '../../types';

const useDataTarget = (dataTarget: NewDataTarget) => {
  const table = {
    name: dataTarget.dataLeaf.leaf?.name || '',
    schema: dataTarget.dataLeaf.name,
  };

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
  } = useMetadataTablePermissions(table, dataTarget.dataSource.database);

  const isLoading = permissionsLoading || metadataLoading;
  const isError = permissionsError || metadataError;

  return {
    dataTarget,
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

export const useBulkDeletePermissions = (dataTarget: NewDataTarget) => {
  const {
    resourceVersion,
    permissions,
    isLoading: dataTargetLoading,
    isError: dataTargetError,
  } = useDataTarget(dataTarget);

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
      dataTarget,
      roleName: '',
      resourceVersion,
      roleList,
    });

    await mutateAsync({
      query: body,
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
