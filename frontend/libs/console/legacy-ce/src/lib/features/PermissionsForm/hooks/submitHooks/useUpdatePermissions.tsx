import { useSubmitForm } from './useSubmitForm';
import { useDeletePermission } from './useDeletePermission';

import { AccessType, QueryType } from '../../types';

export interface UseUpdatePermissionsArgs {
  dataSourceName: string;
  table: unknown;
  roleName: string;
  queryType: QueryType;
  accessType: AccessType;
}

export const useUpdatePermissions = ({
  dataSourceName,
  table,
  roleName,
  queryType,
  accessType,
}: UseUpdatePermissionsArgs) => {
  const updatePermissions = useSubmitForm({
    dataSourceName,
    table,
    roleName,
    queryType,
    accessType,
  });

  const deletePermissions = useDeletePermission({
    dataSourceName,
    table,
    roleName,
  });

  return {
    updatePermissions,
    deletePermissions,
  };
};
