import defaultState, { RemoteSchemaPermissionsState } from './state';
import { Dispatch } from '../../../../types';
import { updateBulkSelect } from './utils';
import {
  PERMISSIONS_OPEN_EDIT,
  PERMISSIONS_CLOSE_EDIT,
  SET_ROLE_NAME,
  SET_DEFAULTS,
  SET_SCHEMA_DEFINITION,
  PERM_DESELECT_BULK,
  PERM_SELECT_BULK,
  PERM_RESET_BULK_SELECT,
  MAKE_REQUEST,
  REQUEST_FAILURE,
  REQUEST_SUCCESS,
  PermOpenEdit,
  PermCloseEdit,
  PermSetRoleName,
  SetDefaults,
  SetSchemaDefinition,
  PermSelectBulk,
  PermDeslectBulk,
  PermResetBulkSelect,
  MakeRequest,
  SetRequestFailure,
  SetRequestSuccess,
  RSPEvents,
} from './types';

export const permOpenEdit = (
  role: string,
  isNewRole: boolean,
  isNewPerm: boolean
): PermOpenEdit => ({
  type: PERMISSIONS_OPEN_EDIT,
  role,
  isNewRole,
  isNewPerm,
});
export const permCloseEdit = (): PermCloseEdit => ({
  type: PERMISSIONS_CLOSE_EDIT,
});
export const permSetRoleName = (rolename: string): PermSetRoleName => ({
  type: SET_ROLE_NAME,
  rolename,
});
export const setDefaults = (): SetDefaults => ({
  type: SET_DEFAULTS,
});
export const setSchemaDefinition = (
  definition: string
): SetSchemaDefinition => ({
  type: SET_SCHEMA_DEFINITION,
  definition,
});
export const permSelectBulk = (selectedRole: string): PermSelectBulk => ({
  type: PERM_SELECT_BULK,
  selectedRole,
});
export const permDeslectBulk = (selectedRole: string): PermDeslectBulk => ({
  type: PERM_DESELECT_BULK,
  selectedRole,
});
export const permResetBulkSelect = (): PermResetBulkSelect => ({
  type: PERM_RESET_BULK_SELECT,
});
export const makeRequest = (): MakeRequest => ({ type: MAKE_REQUEST });
export const setRequestSuccess = (): SetRequestSuccess => ({
  type: REQUEST_SUCCESS,
});
export const setRequestFailure = (): SetRequestFailure => ({
  type: REQUEST_FAILURE,
});
export const permSetBulkSelect = (isChecked: boolean, selectedRole: string) => {
  return (dispatch: Dispatch) => {
    if (isChecked) {
      dispatch(permSelectBulk(selectedRole));
    } else {
      dispatch(permDeslectBulk(selectedRole));
    }
  };
};

const reducer = (
  state = defaultState,
  action: RSPEvents
): RemoteSchemaPermissionsState => {
  switch (action.type) {
    case MAKE_REQUEST:
      return {
        ...state,
        isFetching: true,
      };
    case REQUEST_SUCCESS:
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
        schemaDefinition: action.definition,
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
    case PERM_RESET_BULK_SELECT:
      return {
        ...state,
        bulkSelect: [],
      };
    case SET_DEFAULTS:
      return defaultState;
    default:
      return state;
  }
};

export default reducer;
