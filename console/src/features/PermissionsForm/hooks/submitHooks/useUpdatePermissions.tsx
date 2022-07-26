import { useSubmitForm } from './useSubmitForm';
import { useDeletePermission } from './useDeletePermission';

import { AccessType, QueryType } from '../../types';
import { NewDataTarget } from '../../../PermissionsTab/types/types';

export interface UseUpdatePermissionsArgs {
  dataTarget: NewDataTarget;
  roleName: string;
  queryType: QueryType;
  accessType: AccessType;
}

export const useUpdatePermissions = ({
  dataTarget,
  roleName,
  queryType,
  accessType,
}: UseUpdatePermissionsArgs) => {
  const updatePermissions = useSubmitForm({
    dataTarget,
    roleName,
    queryType,
    accessType,
  });

  const deletePermissions = useDeletePermission({
    dataTarget,
    roleName,
  });

  return { updatePermissions, deletePermissions };
};
