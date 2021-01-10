import { listState } from './state';
import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import requestAction from '../../../utils/requestAction';
import dataHeaders from '../Data/Common/Headers';
import globals from '../../../Globals';
import returnMigrateUrl from '../Data/Common/getMigrateUrl';
import { CLI_CONSOLE_MODE, SERVER_CONSOLE_MODE } from '../../../constants';
import { loadMigrationStatus } from '../../Main/Actions';
import { handleMigrationErrors } from '../../../utils/migration';
import { showSuccessNotification } from '../Common/Notification';
import { makeMigrationCall, fetchRoleList } from '../Data/DataActions';
import { getConfirmation } from '../../Common/utils/jsUtils';
import {
  makeRequest as makePermRequest,
  setRequestSuccess as setPermRequestSuccess,
  setRequestFailure as setPermRequestFailure,
  permSetRoleName,
  permCloseEdit,
  permResetBulkSelect,
} from './Permissions/reducer';
import {
  getRemoteSchemaPermissionQueries,
  getCreateRemoteSchemaPermissionQuery,
  getDropRemoteSchemaPermissionQuery,
} from './Permissions/utils';
import Migration from '../../../utils/migration/Migration';

/* Action constants */

const FETCH_REMOTE_SCHEMAS = '@remoteSchema/FETCH_REMOTE_SCHEMAS';
const REMOTE_SCHEMAS_FETCH_SUCCESS =
  '@remoteSchema/REMOTE_SCHEMAS_FETCH_SUCCESS';
const FILTER_REMOTE_SCHEMAS = '@remoteSchema/FILTER_REMOTE_SCHEMAS';
const REMOTE_SCHEMAS_FETCH_FAIL = '@remoteSchema/REMOTE_SCHEMAS_FETCH_FAIL';
const RESET = '@remoteSchema/RESET';
const SET_CONSISTENT_REMOTE_SCHEMAS =
  '@remoteSchema/SET_CONSISTENT_REMOTE_SCHEMAS';

const VIEW_REMOTE_SCHEMA = '@remoteSchema/VIEW_REMOTE_SCHEMA';

/* */

const fetchRemoteSchemas = () => {
  return (dispatch, getState) => {
    const url = Endpoints.getSchema;
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify({
        type: 'select',
        args: {
          table: {
            name: 'remote_schemas',
            schema: 'hdb_catalog',
          },
          columns: ['*.*'],
          order_by: [{ column: 'name', type: 'asc', nulls: 'last' }],
        },
      }),
    };
    dispatch({ type: FETCH_REMOTE_SCHEMAS });
    return dispatch(requestAction(url, options)).then(
      data => {
        dispatch({
          type: REMOTE_SCHEMAS_FETCH_SUCCESS,
          data,
        });
        return Promise.resolve();
      },
      error => {
        console.error('Failed to load remote schemas' + JSON.stringify(error));
        dispatch({ type: REMOTE_SCHEMAS_FETCH_FAIL, data: error });
        return Promise.reject();
      }
    );
  };
};

const setConsistentRemoteSchemas = data => ({
  type: SET_CONSISTENT_REMOTE_SCHEMAS,
  data,
});

const listReducer = (state = listState, action) => {
  switch (action.type) {
    case FETCH_REMOTE_SCHEMAS:
      return {
        ...state,
        isRequesting: true,
        isError: false,
      };

    case REMOTE_SCHEMAS_FETCH_SUCCESS:
      return {
        ...state,
        remoteSchemas: action.data,
        isRequesting: false,
        isError: false,
      };

    case REMOTE_SCHEMAS_FETCH_FAIL:
      return {
        ...state,
        remoteSchemas: [],
        isRequesting: false,
        isError: action.data,
      };
    case FILTER_REMOTE_SCHEMAS:
      return {
        ...state,
        ...action.data,
      };
    case RESET:
      return {
        ...listState,
      };
    case VIEW_REMOTE_SCHEMA:
      return {
        ...state,
        viewRemoteSchema: action.data,
      };
    case SET_CONSISTENT_REMOTE_SCHEMAS:
      return {
        ...state,
        remoteSchemas: action.data,
      };
    default:
      return {
        ...state,
      };
  }
};

/* makeRequest function to identify what the current mode is and send normal query or a call */
const makeRequest = (
  upQueries,
  downQueries,
  migrationName,
  customOnSuccess,
  customOnError,
  requestMsg,
  successMsg,
  errorMsg
) => {
  return (dispatch, getState) => {
    const upQuery = {
      type: 'bulk',
      args: upQueries,
    };

    const downQuery = {
      type: 'bulk',
      args: downQueries,
    };

    const migrationBody = {
      name: migrationName,
      up: upQuery.args,
      down: downQuery.args,
    };

    const currMigrationMode = getState().main.migrationMode;

    const migrateUrl = returnMigrateUrl(currMigrationMode);

    let finalReqBody;
    if (globals.consoleMode === SERVER_CONSOLE_MODE) {
      finalReqBody = upQuery;
    } else if (globals.consoleMode === CLI_CONSOLE_MODE) {
      finalReqBody = migrationBody;
    }
    const url = migrateUrl;
    const options = {
      method: 'POST',
      credentials: globalCookiePolicy,
      headers: dataHeaders(getState),
      body: JSON.stringify(finalReqBody),
    };

    const onSuccess = data => {
      if (globals.consoleMode === CLI_CONSOLE_MODE) {
        dispatch(loadMigrationStatus()); // don't call for server mode
      }
      if (successMsg) {
        dispatch(showSuccessNotification(successMsg));
      }
      customOnSuccess(data);
    };

    const onError = err => {
      dispatch(handleMigrationErrors(errorMsg, err));
      customOnError(err);
    };

    dispatch(showSuccessNotification(requestMsg));
    return dispatch(requestAction(url, options)).then(onSuccess, onError);
  };
};

const saveRemoteSchemaPermission = (successCb, errorCb) => {
  return (dispatch, getState) => {
    const {
      listData: {
        remoteSchemas: allRemoteSchemas,
        viewRemoteSchema: currentRemoteSchemaName,
      },
      permissions: { permissionEdit, schemaDefinition },
    } = getState().remoteSchemas;

    const currentRemoteSchema = allRemoteSchemas.find(
      rs => rs.name === currentRemoteSchemaName
    );
    const allPermissions = currentRemoteSchema.permissions;

    const { upQueries, downQueries } = getRemoteSchemaPermissionQueries(
      permissionEdit,
      allPermissions,
      currentRemoteSchemaName,
      schemaDefinition
    );

    const migrationName = `save_remote_schema_permission`;
    const requestMsg = 'Saving permission...';
    const successMsg = 'Permission saved successfully';
    const errorMsg = 'Saving permission failed';

    const customOnSuccess = () => {
      dispatch(fetchRemoteSchemas());
      dispatch(fetchRoleList());
      dispatch(setPermRequestSuccess());
      if (successCb) {
        successCb();
      }
    };
    const customOnError = () => {
      dispatch(setPermRequestFailure());
      if (errorCb) {
        errorCb();
      }
    };

    dispatch(makePermRequest());
    makeMigrationCall(
      dispatch,
      getState,
      upQueries,
      downQueries,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const removeRemoteSchemaPermission = (successCb, errorCb) => {
  return (dispatch, getState) => {
    const isOk = getConfirmation(
      'This will remove the permission for this role'
    );
    if (!isOk) return;

    const {
      listData: { viewRemoteSchema: currentRemoteSchema },
      permissions: { permissionEdit, schemaDefinition },
    } = getState().remoteSchemas;

    const { role } = permissionEdit;

    const upQuery = getDropRemoteSchemaPermissionQuery(
      role,
      currentRemoteSchema
    );
    const downQuery = getCreateRemoteSchemaPermissionQuery(
      { role },
      currentRemoteSchema,
      schemaDefinition
    );

    const migrationName = 'remove_remoteSchema_perm';
    const requestMsg = 'Removing permission...';
    const successMsg = 'Permission removed successfully';
    const errorMsg = 'Removing permission failed';

    const customOnSuccess = () => {
      dispatch(fetchRemoteSchemas());
      dispatch(fetchRoleList());
      dispatch(setPermRequestSuccess());
      if (successCb) {
        successCb();
      }
    };
    const customOnError = () => {
      dispatch(setPermRequestFailure());
      if (errorCb) {
        errorCb();
      }
    };

    dispatch(makePermRequest());
    makeMigrationCall(
      dispatch,
      getState,
      [upQuery],
      [downQuery],
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const permRemoveMultipleRoles = () => {
  return (dispatch, getState) => {
    const {
      listData: {
        remoteSchemas: allRemoteSchemas,
        viewRemoteSchema: currentRemoteSchemaName,
      },
      permissions: { bulkSelect },
    } = getState().remoteSchemas;

    const currentRemoteSchema = allRemoteSchemas.find(
      rs => rs.name === currentRemoteSchemaName
    );
    const currentPermissions = currentRemoteSchema.permissions;

    const roles = bulkSelect;
    const migration = new Migration();

    roles.map(role => {
      const currentRolePermission = currentPermissions.filter(el => {
        return el.role_name === role;
      });

      const upQuery = getDropRemoteSchemaPermissionQuery(
        role,
        currentRemoteSchemaName
      );
      const downQuery = getCreateRemoteSchemaPermissionQuery(
        { role },
        currentRemoteSchemaName,
        currentRolePermission[0].definition.schema
      );
      migration.add(upQuery, downQuery);
    });

    // Apply migration

    const migrationName = 'bulk_remove_remoteSchema_perm';
    const requestMsg = 'Removing permissions...';
    const successMsg = 'Permission removed successfully';
    const errorMsg = 'Removing permission failed';

    const customOnSuccess = () => {
      dispatch(permSetRoleName(''));
      dispatch(permCloseEdit());
      dispatch(permResetBulkSelect());
      dispatch(fetchRoleList());
    };
    const customOnError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      migration.upMigration,
      migration.downMigration,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export {
  fetchRemoteSchemas,
  FILTER_REMOTE_SCHEMAS,
  VIEW_REMOTE_SCHEMA,
  makeRequest,
  setConsistentRemoteSchemas,
  saveRemoteSchemaPermission,
  removeRemoteSchemaPermission,
  permRemoveMultipleRoles,
};
export default listReducer;
