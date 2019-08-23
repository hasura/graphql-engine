import { permissionState } from '../state';
import { generateCreatePermQuery } from './utils';
import { showErrorNotification } from '../../Common/Notification';
import { parseRemoteRelPermDefinition } from './utils';
import requestAction from '../../../../utils/requestAction';
import endpoints from '../../../../Endpoints';

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

const SET_CURRENT_PERMISSION_EDIT = '@remoteSchema/SET_CURRENT_PERMISSION_EDIT';
export const setCurrentPermissionEdit = (perm, editType) => ({
  type: SET_CURRENT_PERMISSION_EDIT,
  perm,
  editType
})

const CLOSE_PERMISSION_EDIT = '@remoteSchema/CLOSE_PERMISSION_EDIT';
export const closePermissionEdit = () => ({ type: CLOSE_PERMISSION_EDIT});

const CREATE_REMOTE_SCHEMA_PERMISSION_SUCCESS =
  '@remoteSchema/CREATE_REMOTE_SCHEMA_PERMISSION_SUCCESS';
const CREATE_REMOTE_SCHEMA_PERMISSION_FAILURE =
  '@remoteSchema/CREATE_REMOTE_SCHEMA_PERMISSION_FAILURE';

const CREATE_REMOTE_SCHEMA_PERMISSION =
  '@remoteSchema/CREATE_REMOTE_SCHEMA_PERMISSION';
export const createRemoteSchemaPermission = (index, successCb, failureCb) => {
  return (dispatch, getState) => {
    dispatch({ type: CREATE_REMOTE_SCHEMA_PERMISSION });
    const permsState = getState().remoteSchemas.permissions;
    const rolePermState = permsState.rolePermissions[index];
    const remoteSchemaName = permsState.currentRemoteSchemaName;
    if (!rolePermState.role) {
      return dispatch(
        showErrorNotification(
          'Saving permission failed',
          'Please enter a role name'
        )
      );
    }
    const query = generateCreatePermQuery(rolePermState, remoteSchemaName);
    const headers = getState().tables.dataHeaders;
    return dispatch(
      requestAction(endpoints.query, {
        method: 'POST',
        headers,
        body: JSON.stringify(query),
      })
    ).then(
      data => {
        dispatch({ type: CREATE_REMOTE_SCHEMA_PERMISSION_SUCCESS });
        alert('Success!! (temporary alert message)');
        if (successCb) {
          successCb(data);
        }
      },
      error => {
        console.error(error);
        dispatch({ type: CREATE_REMOTE_SCHEMA_PERMISSION_FAILURE, error });
        if (failureCb) {
          failureCb(error);
        }
      }
    );
  };
};

const reducer = (state = permissionState, action) => {

  switch (action.type) {
    case SET_CURRENT_REMOTE_SCHEMA:
      return {
        ...state,
        currentRemoteSchemaName: action.currentRemoteSchemaName,
      };

    case SET_PERMISSION_ROLE:
      return {
        ...state,
        editState: {
          ...state.editState,
          role: action.role
        }
      };

    case SET_PERMISSION_TYPES:
      return {
        ...state,
        editState: {
          ...state.editState,
          allowedTypes: action.allowedTypes
        }
      };

    case CREATE_REMOTE_SCHEMA_PERMISSION_SUCCESS:
      return {
        ...state,
        isFetching: false,
      };

    case SET_CURRENT_PERMISSION_EDIT:
      return {
        ...state,
        editState: {
          ...action.perm,
          isEditing: true,
          editType: action.editType
        }
      }

    case CLOSE_PERMISSION_EDIT:
      return {
        ...state,
        editState: {
          ...permissionState.editState
        }
      }

    default:
      return {
        ...state,
      };
  }
};

export default reducer;
