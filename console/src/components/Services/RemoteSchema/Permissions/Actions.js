import { permissionState } from '../state';
import { generateCreatePermQuery } from './utils';
import { showErrorNotification } from '../../Common/Notification';
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

    case CREATE_REMOTE_SCHEMA_PERMISSION_SUCCESS:
      return {
        ...state,
        isFetching: false,
      };

    default:
      return {
        ...state,
      };
  }
};

export default reducer;
