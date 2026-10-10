import {
  SelectPermission,
  FunctionPermission,
  AccessType,
} from '@hasura/shared/types';

export const getRoleQueryPermissionSymbol = (
  allPermissions: FunctionPermission[] | undefined | null,
  permissionRole: string,
  selectPermissionsForTable: SelectPermission[] | null | undefined,
  isEditable: boolean,
): AccessType => {
  if (permissionRole === 'admin') {
    return 'fullAccess';
  }

  let isTableSelectPermissionsEnabled = false;
  let isPermissionsEnabledOnMetadata = false;

  // Checking if select permissions are there on the reference table
  if (
    selectPermissionsForTable &&
    selectPermissionsForTable.find(
      (selectPermissionEntry) => selectPermissionEntry.role === permissionRole,
    )
  ) {
    isTableSelectPermissionsEnabled = true;
  }

  // If permissions are inferred and not editable we only need to know if corresponding table has select permissions
  if (!isEditable) {
    return isTableSelectPermissionsEnabled ? 'fullAccess' : 'noAccess';
  }

  // Checking if permissions are enabled and visible in the metadata
  if (findFunctionPermissions(allPermissions, permissionRole)) {
    isPermissionsEnabledOnMetadata = true;
  }

  if (!isPermissionsEnabledOnMetadata) {
    return 'noAccess';
  }

  if (isPermissionsEnabledOnMetadata && !isTableSelectPermissionsEnabled) {
    return 'partialAccessWarning';
  }

  return 'fullAccess';
};

const findFunctionPermissions = (
  allPermissions: FunctionPermission[] | undefined | null,
  userRole: string,
) => {
  if (!allPermissions) {
    return undefined;
  }
  return allPermissions.find((permRole) => permRole.role === userRole);
};
