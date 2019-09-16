import { permissionState } from '../state';
import { generateCreatePermQuery, generateDropPermQuery } from './utils';
import { showErrorNotification } from '../../Common/Notification';
import { fetchRemoteSchemas } from '../Actions';
import { fetchRoleList } from '../../Data/DataActions';
import { makeRequest } from '../Actions';

// set current remote schema
const SET_CURRENT_REMOTE_SCHEMA = '@remoteSchema/SET_CURRENT_REMOTE_SCHEMA';
export const setCurrentRemoteSchema = currentRemoteSchemaName => ({
  type: SET_CURRENT_REMOTE_SCHEMA,
  currentRemoteSchemaName,
});

// set permission role
const SET_PERMISSION_ROLE = '@remoteSchema/SET_PERMISSION_ROLE';
export const setPermissionRole = role => ({
  type: SET_PERMISSION_ROLE,
  role,
});

// set permission types
const SET_PERMISSION_TYPES = '@remoteSchema/SET_PERMISSION_TYPES';
export const setPermissionTypes = allowedTypes => ({
  type: SET_PERMISSION_TYPES,
  allowedTypes,
});

// set permission edit
const SET_CURRENT_PERMISSION_EDIT = '@remoteSchema/SET_CURRENT_PERMISSION_EDIT';
export const setCurrentPermissionEdit = (perm, editType) => ({
  type: SET_CURRENT_PERMISSION_EDIT,
  perm,
  editType,
});

// close permissions edit
const CLOSE_PERMISSION_EDIT = '@remoteSchema/CLOSE_PERMISSION_EDIT';
export const closePermissionEdit = () => ({ type: CLOSE_PERMISSION_EDIT });

// reset permissiosn state
const RESET_PERMISSIONS_STATE = '@remoteSchema/RESET_PERMISSIONS_STATE';
export const resetPermState = () => ({ type: RESET_PERMISSIONS_STATE });

// drop remote schema permission
const DROP_REMOTE_SCHEMA_PERMISSION =
  '@remoteSchema/DROP_REMOTE_SCHEMA_PERMISSION';
const DROP_REMOTE_SCHEMA_PERMISSION_SUCCESS =
  '@remoteSchema/DROP_REMOTE_SCHEMA_PERMISSION_SUCCESS';
const DROP_REMOTE_SCHEMA_PERMISSION_FAILURE =
  '@remoteSchema/DROP_REMOTE_SCHEMA_PERMISSION_FAILURE';

export const deleteRemoteSchemaPermission = (successCb, failureCb) => {
  return (dispatch, getState) => {
    const permState = getState().remoteSchemas.permissions;
    const { editState, currentRemoteSchemaName } = permState;
    const upQuery = [
      generateDropPermQuery(editState.role, currentRemoteSchemaName),
    ];

    // get existing permissions for the given role for the current remote schema
    const existingPermissions = getState().remoteSchemas.listData.remoteSchemas.find(
      r => r.name === currentRemoteSchemaName
    );
    const existingRolePerm = existingPermissions.permissions.find(
      p => p.role === editState.role
    );

    // generate permissions down query (create the permission)
    const downQuery = [
      generateCreatePermQuery(existingRolePerm, currentRemoteSchemaName, true),
    ];

    // confirm deletion action
    const isOk = window.confirm('Are you absolutely sure?');
    if (!isOk) return;

    dispatch({ type: DROP_REMOTE_SCHEMA_PERMISSION });

    const migrationName = `drop_remote_schema_${currentRemoteSchemaName}_permission_${
      editState.role
    }`;

    const customOnSuccess = () => {
      dispatch({ type: DROP_REMOTE_SCHEMA_PERMISSION_SUCCESS });
      dispatch(fetchRemoteSchemas());
      dispatch(fetchRoleList());
      dispatch(closePermissionEdit());
      if (successCb) {
        successCb();
      }
    };

    const customOnError = () => {
      dispatch({ type: DROP_REMOTE_SCHEMA_PERMISSION_FAILURE });
      if (failureCb) {
        failureCb();
      }
    };

    const requestMsg = 'Deleting permission...';
    const successMsg = 'Deleting permission successful';
    const errorMsg = 'Deleting permission failed';

    return dispatch(
      makeRequest(
        upQuery,
        downQuery,
        migrationName,
        customOnSuccess,
        customOnError,
        requestMsg,
        successMsg,
        errorMsg
      )
    );
  };
};

// create remote schema (also reused for edit_remote_schema)
const CREATE_REMOTE_SCHEMA_PERMISSION_SUCCESS =
  '@remoteSchema/CREATE_REMOTE_SCHEMA_PERMISSION_SUCCESS';
const CREATE_REMOTE_SCHEMA_PERMISSION_FAILURE =
  '@remoteSchema/CREATE_REMOTE_SCHEMA_PERMISSION_FAILURE';
const CREATE_REMOTE_SCHEMA_PERMISSION =
  '@remoteSchema/CREATE_REMOTE_SCHEMA_PERMISSION';
export const createRemoteSchemaPermission = (successCb, failureCb) => {
  return (dispatch, getState) => {
    const permsState = getState().remoteSchemas.permissions;
    const editState = permsState.editState;
    const remoteSchemaName = permsState.currentRemoteSchemaName;

    const existingPermissions = getState().remoteSchemas.listData.remoteSchemas.find(
      r => r.name === remoteSchemaName
    );
    const existingRolePerm = existingPermissions.permissions.find(
      p => p.role === editState.role
    );

    if (editState.isNew) {
      if (!editState.newRole && !editState.role) {
        return dispatch(
          showErrorNotification(
            'Saving permission failed',
            'Please enter a role name'
          )
        );
      }
      if (getState().tables.allRoles.includes(editState.newRole)) {
        return dispatch(
          showErrorNotification(
            'Saving permission failed',
            'This role name already exists'
          )
        );
      }
    }

    const createQuery = generateCreatePermQuery(
      editState,
      remoteSchemaName,
      false
    );

    const downQuery = [];
    const upQuery = [];

    if (editState.isNew) {
      upQuery.push(createQuery);
      downQuery.push(
        generateDropPermQuery(editState.newRole, remoteSchemaName)
      );
    } else {
      upQuery.push(generateDropPermQuery(editState.role, remoteSchemaName));
      upQuery.push(createQuery);
      downQuery.push(
        generateCreatePermQuery(existingRolePerm, remoteSchemaName, true)
      );
    }

    const migrationName = `create_remote_schema_${remoteSchemaName}_permission_${editState.newRole ||
      editState.role}`;
    const customOnSuccess = () => {
      dispatch({ type: CREATE_REMOTE_SCHEMA_PERMISSION_SUCCESS });
      dispatch(fetchRemoteSchemas());
      dispatch(fetchRoleList());
      dispatch(closePermissionEdit());
      if (successCb) {
        successCb();
      }
    };

    const customOnError = () => {
      dispatch({ type: CREATE_REMOTE_SCHEMA_PERMISSION_FAILURE });
      if (failureCb) {
        failureCb();
      }
    };

    const permAction = editState.isNew ? 'Creating' : 'Updating';

    const requestMsg = `${permAction} permission...`;
    const successMsg = `${permAction} permission successful`;
    const errorMsg = `${permAction} permission failed`;

    dispatch({ type: CREATE_REMOTE_SCHEMA_PERMISSION });

    return dispatch(
      makeRequest(
        upQuery,
        downQuery,
        migrationName,
        customOnSuccess,
        customOnError,
        requestMsg,
        successMsg,
        errorMsg
      )
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
          newRole: action.role,
        },
      };

    case SET_PERMISSION_TYPES:
      return {
        ...state,
        editState: {
          ...state.editState,
          allowedTypes: action.allowedTypes,
        },
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
        isFetching: true,
      };

    case DROP_REMOTE_SCHEMA_PERMISSION_SUCCESS:
      return {
        ...state,
        isFetching: false,
      };

    case DROP_REMOTE_SCHEMA_PERMISSION_FAILURE:
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
          editType: action.editType,
        },
      };

    case CLOSE_PERMISSION_EDIT:
      return {
        ...state,
        editState: {
          ...permissionState.editState,
        },
      };

    case RESET_PERMISSIONS_STATE:
      return JSON.parse(JSON.stringify(permissionState));

    default:
      return {
        ...state,
      };
  }
};

export default reducer;
