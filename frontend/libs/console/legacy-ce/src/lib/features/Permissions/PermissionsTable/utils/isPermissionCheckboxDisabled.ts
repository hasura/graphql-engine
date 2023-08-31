import { RolePermission } from '../hooks';

export const isPermissionCheckboxDisabled = (
  permissionTypes: RolePermission['permissionTypes']
) => !permissionTypes.some(({ access }) => access !== 'noAccess');
