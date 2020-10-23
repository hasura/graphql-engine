import defaultState from './state';

const SET_REMOTESCHEMA_PERMISSIONS =
  'RemoteSchemas/Permissions/SET_REMOTESCHEMA_PERMISSIONS';

export const setRemoteSchemaPermission = perms => ({
  type: SET_REMOTESCHEMA_PERMISSIONS,
  perms,
});

const PERMISSIONS_OPEN_EDIT = 'RemoteSchemas/Permissions/PERMISSIONS_OPEN_EDIT';
export const permOpenEdit = (role, isNewRole, isNewPerm) => ({
  type: PERMISSIONS_OPEN_EDIT,
  role,
  isNewRole,
  isNewPerm,
});

const PERMISSIONS_CLOSE_EDIT =
  'RemoteSchemas/Permissions/PERMISSIONS_CLOSE_EDIT';
export const permCloseEdit = () => ({
  type: PERMISSIONS_CLOSE_EDIT,
});

const SET_ROLE_NAME = 'RemoteSchemas/Permissions/SET_ROLE_NAME';
export const permSetRoleName = (rolename: string) => ({
  type: SET_ROLE_NAME,
  rolename,
});

const SET_DEFAULTS = 'RemoteSchemas/Permissions/SET_DEFAULTS';
export const setDefaults = () => ({
  type: SET_DEFAULTS,
});

const SET_SCHEMA_DEFINITION = 'RemoteSchemas/Permissions/SET_SCHEMA_DEFINITION';
export const setSchemaDefinition = (sdl, error = null, timer, ast) => ({
  type: SET_SCHEMA_DEFINITION,
  definition: { sdl, error, timer, ast },
});

export const PERM_SELECT_BULK = 'RemoteSchemas/Permissions/PERM_SELECT_BULK';
export const permSelectBulk = selectedRole => ({
  type: PERM_SELECT_BULK,
  selectedRole,
});

export const PERM_DESELECT_BULK =
  'RemoteSchemas/Permissions/PERM_DESELECT_BULK';
export const permDeslectBulk = selectedRole => ({
  type: PERM_DESELECT_BULK,
  selectedRole,
});

export const PERM_RESET_BULK_SELECT =
  'RemoteSchemas/Permissions/PERM_RESET_BULK_SELECT';

export const permSetBulkSelect = (isChecked, selectedRole) => {
  return dispatch => {
    if (isChecked) {
      dispatch({ type: PERM_SELECT_BULK, data: selectedRole });
    } else {
      dispatch({ type: PERM_DESELECT_BULK, data: selectedRole });
    }
  };
};

export const updateBulkSelect = (bulkSelect, selectedRole, isAdd) => {
  let bulkRes = bulkSelect;
  if (isAdd) {
    bulkRes.push(selectedRole);
  } else {
    bulkRes = bulkRes.filter(e => e !== selectedRole);
  }
  return bulkRes;
};

const MAKE_REQUEST = 'RemoteSchemas/Permissions/MAKE_REQUEST';
export const makeRequest = () => ({ type: MAKE_REQUEST });
const REQUEST_SUCCESS = 'RemoteSchemas/Permissions/REQUEST_SUCCESS';
export const setRequestSuccess = () => ({ type: REQUEST_SUCCESS });
const REQUEST_FAILURE = 'RemoteSchemas/Permissions/REQUEST_FAILURE';
export const setRequestFailure = () => ({ type: REQUEST_FAILURE });

const reducer = (state = defaultState, action) => {
  switch (action.type) {
    case MAKE_REQUEST:
      return {
        ...state,
        isFetching: true,
      };
    case REQUEST_SUCCESS:
      return {
        ...state,
        isFetching: false,
      };
    case REQUEST_FAILURE:
      return {
        ...state,
        isFetching: false,
      };
    case PERMISSIONS_OPEN_EDIT:
      return {
        ...state,
        isEditing: true,
        permissionEdit: {
          ...state.permissionEdit,
          isNewRole: !!action.isNewRole,
          isNewPerm: !!action.isNewPerm,
          role: action.role,
          filter: {},
        },
      };
    case PERMISSIONS_CLOSE_EDIT:
      return {
        ...state,
        isEditing: false,
        permissionEdit: { ...defaultState.permissionEdit },
      };
    case SET_SCHEMA_DEFINITION:
      return {
        ...state,
        schemaDefinition: {
          ...action.definition,
          sdl:
            action.definition.sdl !== null
              ? action.definition.sdl
              : state.schemaDefinition.sdl,
        },
      };
    case SET_ROLE_NAME:
      return {
        ...state,
        permissionEdit: {
          ...state.permissionEdit,
          newRole: action.rolename,
        },
      };
    case PERM_SELECT_BULK:
      return {
        ...state,
        bulkSelect: updateBulkSelect(
          state.bulkSelect,
          action.selectedRole,
          true
        ),
      };
    case PERM_DESELECT_BULK:
      return {
        ...state,
        bulkSelect: updateBulkSelect(
          state.bulkSelect,
          action.selectedRole,
          false
        ),
      };
    case SET_DEFAULTS:
      return defaultState;
    default:
      return state;
  }
};

export default reducer;
