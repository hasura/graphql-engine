import defaultState from './state';

const SET_ACTION_PERMISSIONS = 'Actions/Permissions/SET_ACTION_PERMISSIONS';

export const setActionPermissiosn = perms => ({
  type: SET_ACTION_PERMISSIONS,
  perms,
});

const PERMISSIONS_OPEN_EDIT = 'Actions/Permissions/PERMISSIONS_OPEN_EDIT';
export const permOpenEdit = (role, isNewRole, isNewPerm) => ({
  type: PERMISSIONS_OPEN_EDIT,
  role,
  isNewRole,
  isNewPerm,
});

const PERMISSIONS_CLOSE_EDIT = 'Actions/Permissions/PERMISSIONS_CLOSE_EDIT';
export const permCloseEdit = () => ({
  type: PERMISSIONS_CLOSE_EDIT,
});

const SET_ROLE_NAME = 'Actions/Permissions/SET_ROLE_NAME';
export const permSetRoleName = rolename => ({
  type: SET_ROLE_NAME,
  rolename,
});

const SET_DEFAULTS = 'Actions/Permissions/SET_DEFAULTS';
export const setDefaults = () => ({
  type: SET_DEFAULTS,
});

const MAKE_REQUEST = 'Actions/Permissions/MAKE_REQUEST';
export const makePermRequest = () => ({ type: MAKE_REQUEST });
const REQUEST_SUCCESS = 'Actions/Permissions/REQUEST_SUCCESS';
export const setRequestSuccess = () => ({ type: REQUEST_SUCCESS });
const REQUEST_FAILURE = 'Actions/Permissions/REQUEST_FAILURE';
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
    case SET_ROLE_NAME:
      return {
        ...state,
        permissionEdit: {
          ...state.permissionEdit,
          newRole: action.rolename,
        },
      };
    case SET_DEFAULTS:
      return defaultState;
    default:
      return state;
  }
};

export default reducer;
