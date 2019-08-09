import { permissionState } from '../state';

const SET_CURRENT_REMOTE_SCHEMA = '@remoteSchema/SET_CURRENT_REMOTE_SCHEMA';
export const setCurrentRemoteSchema = currentRemoteSchemaName => ({
  type: SET_CURRENT_REMOTE_SCHEMA,
  currentRemoteSchemaName,
});

const SET_PERMISSION_ROLE = '@remoteSchema/SET_PERMISSION_ROLE';
export const setPermissionRole = (role, index) => ({
  type: SET_PERMISSION_ROLE,
  role,
  index,
});

const SET_PERMISSION_TYPES = '@remoteSchema/SET_PERMISSION_TYPES';
export const setPermissionTypes = (types, index) => ({
  type: SET_PERMISSION_TYPES,
  types,
  index,
});

const reducer = (state = permissionState, action) => {
  const newRolePermissions = JSON.parse(JSON.stringify(state.rolePermissions));

  switch (action.type) {
    case SET_CURRENT_REMOTE_SCHEMA:
      return {
        ...state,
        currentRemoteSchemaName: action.currentRemoteSchemaName,
      };

    case SET_PERMISSION_ROLE:
      newRolePermissions[action.index].role = action.role;
      return {
        ...state,
        rolePermissions: newRolePermissions,
      };

    case SET_PERMISSION_TYPES:
      newRolePermissions[action.index].allowedTypes = action.types;
      return {
        ...state,
        rolePermissions: newRolePermissions,
      };

    default:
      return {
        ...state,
      };
  }
};

export default reducer;
