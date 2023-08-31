import { RolePermission } from '../hooks';
import { isPermissionCheckboxDisabled } from './isPermissionCheckboxDisabled';

describe('isPermissionCheckboxDisabled', () => {
  it('returns false', () => {
    const permissionTypes: RolePermission['permissionTypes'] = [
      {
        permissionType: 'insert',
        access: 'fullAccess',
      },
      {
        permissionType: 'select',
        access: 'noAccess',
      },
    ];
    expect(isPermissionCheckboxDisabled(permissionTypes)).toBeFalsy();
  });

  it('returns true', () => {
    const permissionTypes: RolePermission['permissionTypes'] = [
      {
        permissionType: 'insert',
        access: 'noAccess',
      },
      {
        permissionType: 'select',
        access: 'noAccess',
      },
    ];
    expect(isPermissionCheckboxDisabled(permissionTypes)).toBeTruthy();
  });
});
