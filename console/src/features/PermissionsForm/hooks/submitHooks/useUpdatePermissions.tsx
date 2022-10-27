import { useSubmitForm } from './useSubmitForm';
import { useDeletePermission } from './useDeletePermission';

import { AccessType, QueryType } from '../../types';

export interface UseUpdatePermissionsArgs {
  currentSource: string;
  dataSourceName: string;
  table: unknown;
  roleName: string;
  queryType: QueryType;
  accessType: AccessType;
}

export const useUpdatePermissions = ({
  currentSource,
  dataSourceName,
  table,
  roleName,
  queryType,
  accessType,
}: UseUpdatePermissionsArgs) => {
  const updatePermissions = useSubmitForm({
    currentSource,
    dataSourceName,
    table,
    roleName,
    queryType,
    accessType,
  });

  const deletePermissions = useDeletePermission({
    currentSource,
    dataSourceName,
    table,
    roleName,
  });

  return {
    updatePermissions,
    deletePermissions,
  };
};
