import { permissionState } from '../state';
import { generateCreatePermQuery, generateDropPermQuery } from './utils';
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
export const setPermissionRole = (role) => ({
  type: SET_PERMISSION_ROLE,
  role,
});

const SET_PERMISSION_TYPES = '@remoteSchema/SET_PERMISSION_TYPES';
export const setPermissionTypes = (allowedTypes) => ({
  type: SET_PERMISSION_TYPES,
  allowedTypes
});

const SET_CURRENT_PERMISSION_EDIT = '@remoteSchema/SET_CURRENT_PERMISSION_EDIT';
export const setCurrentPermissionEdit = (perm, editType) => ({
  type: SET_CURRENT_PERMISSION_EDIT,
  perm,
  editType
})

const CLOSE_PERMISSION_EDIT = '@remoteSchema/CLOSE_PERMISSION_EDIT';
export const closePermissionEdit = () => ({ type: CLOSE_PERMISSION_EDIT});

const DROP_REMOTE_SCHEMA_PERMISSION = '@remoteSchema/DROP_REMOTE_SCHEMA_PERMISSION';
const DROP_REMOTE_SCHEMA_PERMISSION_SUCCESS = '@remoteSchema/DROP_REMOTE_SCHEMA_PERMISSION_SUCCESS';
const DROP_REMOTE_SCHEMA_PERMISSION_FAILURE = '@remoteSchema/DROP_REMOTE_SCHEMA_PERMISSION_FAILURE'

export const deleteRemoteSchemaPermission = (successCb, failureCb) => {
  return (dispatch, getState) => {
    const permState = getState().remoteSchemas.permissions;
    const { editState, currentRemoteSchemaName } = permState;
    const query = generateDropPermQuery(editState.role, currentRemoteSchemaName);
    const headers = getState().tables.dataHeaders;

    const isOk = window.confirm('Are you absolutely sure?');
    if (!isOk) return;

    dispatch({type: DROP_REMOTE_SCHEMA_PERMISSION});
    return dispatch(
      requestAction(endpoints.query, {
        method: 'POST',
        headers,
        body: JSON.stringify(query),
      })
    ).then(
      data => {
        dispatch({ type: DROP_REMOTE_SCHEMA_PERMISSION_SUCCESS});
        dispatch(fetchRemoteSchemaPermissions());
        dispatch(closePermissionEdit());
        if (successCb) {
          successCb(data);
        }
      },
      error => {
        console.error(error);
        dispatch({ type: DROP_REMOTE_SCHEMA_PERMISSION_FAILURE, error });
        if (failureCb) {
          failureCb(error);
        }
      }
    );
  }
}

const CREATE_REMOTE_SCHEMA_PERMISSION_SUCCESS =
  '@remoteSchema/CREATE_REMOTE_SCHEMA_PERMISSION_SUCCESS';
const CREATE_REMOTE_SCHEMA_PERMISSION_FAILURE =
  '@remoteSchema/CREATE_REMOTE_SCHEMA_PERMISSION_FAILURE';

const CREATE_REMOTE_SCHEMA_PERMISSION =
  '@remoteSchema/CREATE_REMOTE_SCHEMA_PERMISSION';
export const createRemoteSchemaPermission = (successCb, failureCb) => {
  return (dispatch, getState) => {
    dispatch({ type: CREATE_REMOTE_SCHEMA_PERMISSION });
    const permsState = getState().remoteSchemas.permissions;
    const editState = permsState.editState;
    const remoteSchemaName = permsState.currentRemoteSchemaName;
    if (!editState.role) {
      return dispatch(
        showErrorNotification(
          'Saving permission failed',
          'Please enter a role name'
        )
      );
    }

    if (getState().tables.allRoles.includes(editState.role)) {
      return dispatch(
        showErrorNotification(
          'Saving permission failed',
          'This role name already exists'
        )
      );
    }

    const query = generateCreatePermQuery(editState, remoteSchemaName);
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
        dispatch(fetchRemoteSchemaPermissions());
        dispatch(closePermissionEdit());
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

const SET_REMOTE_SCHEMA_PERMISSIONS = '@remoteSchema/SET_REMOTE_SCHEMA_PERMISSIONS';
const setRemoteSchemaPermissions = (perms) => ({
  type: SET_REMOTE_SCHEMA_PERMISSIONS,
  perms
});

export const fetchRemoteSchemaPermissions = () => (dispatch, getState) => {
  const query = {
    "type": "select",
    "args": {
      "table": {
        "schema": "hdb_catalog",
        "name": "remote_schema_permissions"
      },
      "columns": ["*.*"]
    }
  };
  const options = {
    method: 'POST',
    headers: getState().tables.dataHeaders,
    body: JSON.stringify(query),
  };
  return dispatch(requestAction(endpoints.query, options))
    .then(data => {
      dispatch(setRemoteSchemaPermissions(data));
    })
    .catch (err => {
      console.error(error);
    })
}

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


    case CREATE_REMOTE_SCHEMA_PERMISSION:
      return {
        ...state,
        isFetching: true,
      };

    case CREATE_REMOTE_SCHEMA_PERMISSION_SUCCESS:
      return {
        ...state,
        isFetching: false,
      };

    case DROP_REMOTE_SCHEMA_PERMISSION:
      return {
        ...state,
        isFetching: true
      }

    case DROP_REMOTE_SCHEMA_PERMISSION_SUCCESS:
      return {
        ...state,
        isFetching: false
      }    

    case DROP_REMOTE_SCHEMA_PERMISSION_FAILURE:
      return {
        ...state,
        isFetching: false
      }    

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

    case SET_REMOTE_SCHEMA_PERMISSIONS:
      return {
        ...state,
        existingPermissions: action.perms
      };

    default:
      return {
        ...state,
      };
  }
};

export default reducer;
