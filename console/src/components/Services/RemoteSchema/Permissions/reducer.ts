import defaultState from './state';
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
  SchemaDefinition,
} from './types';

export const permOpenEdit = (
  role: string,
  isNewRole: boolean,
  isNewPerm: boolean
) => ({
  type: PERMISSIONS_OPEN_EDIT,
  role,
  isNewRole,
  isNewPerm,
});
export const permCloseEdit = () => ({
  type: PERMISSIONS_CLOSE_EDIT,
});
export const permSetRoleName = (rolename: string) => ({
  type: SET_ROLE_NAME,
  rolename,
});
export const setDefaults = () => ({
  type: SET_DEFAULTS,
});
export const setSchemaDefinition = (definition: SchemaDefinition) => ({
  type: SET_SCHEMA_DEFINITION,
  definition,
});
export const permSelectBulk = (selectedRole: string) => ({
  type: PERM_SELECT_BULK,
  selectedRole,
});
export const permDeslectBulk = (selectedRole: string) => ({
  type: PERM_DESELECT_BULK,
  selectedRole,
});
export const permResetBulkSelect = () => ({
  type: PERM_RESET_BULK_SELECT,
});
export const makeRequest = () => ({ type: MAKE_REQUEST });
export const setRequestSuccess = () => ({ type: REQUEST_SUCCESS });
export const setRequestFailure = () => ({ type: REQUEST_FAILURE });
export const permSetBulkSelect = (isChecked: boolean, selectedRole: any) => {
  return (dispatch: Dispatch) => {
    if (isChecked) {
      dispatch(permSelectBulk(selectedRole));
    } else {
      dispatch(permDeslectBulk(selectedRole));
    }
  };
};

const reducer = (state = defaultState, action: any) => {
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
          value:
            action.definition.value !== null
              ? action.definition.value
              : state.schemaDefinition.value,
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
