import { useSubmitForm } from './useSubmitForm';
import { useDeletePermission } from './useDeletePermission';

import { AccessType, QueryType } from '../../types';

export interface UseUpdatePermissionsArgs {
  tableName: string;
  schemaName: string;
  roleName: string;
  queryType: QueryType;
  accessType: AccessType;
}

export const useUpdatePermissions = ({
  tableName,
  schemaName,
  roleName,
  queryType,
  accessType,
}: UseUpdatePermissionsArgs) => {
  const updatePermissions = useSubmitForm({
    tableName,
    schemaName,
    roleName,
    queryType,
    accessType,
  });

  const deletePermissions = useDeletePermission({
    tableName,
    schemaName,
    roleName,
  });

  return { updatePermissions, deletePermissions };
};
