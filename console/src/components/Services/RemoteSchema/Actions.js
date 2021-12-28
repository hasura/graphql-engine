import { listState } from './state';
import { globalCookiePolicy } from '../../../Endpoints';
import requestAction from '../../../utils/requestAction';
import dataHeaders from '../Data/Common/Headers';
import globals from '../../../Globals';
import returnMigrateUrl from '../Data/Common/getMigrateUrl';
import { CLI_CONSOLE_MODE, SERVER_CONSOLE_MODE } from '../../../constants';
import { loadMigrationStatus } from '../../Main/Actions';
import { handleMigrationErrors } from '../../../utils/migration';
import { showSuccessNotification } from '../Common/Notification';
import { makeMigrationCall } from '../Data/DataActions';
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
import { exportMetadata } from '../../../metadata/actions';
import { getRemoteSchemas } from '../../../metadata/selector';

/* Action constants */

const FILTER_REMOTE_SCHEMAS = '@remoteSchema/FILTER_REMOTE_SCHEMAS';
const RESET = '@remoteSchema/RESET';

const VIEW_REMOTE_SCHEMA = '@remoteSchema/VIEW_REMOTE_SCHEMA';
const SET_REMOTE_SCHEMAS = '@remoteSchema/SET_REMOTE_SCHEMAS';

const listReducer = (state = listState, action) => {
  switch (action.type) {
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
    case SET_REMOTE_SCHEMAS:
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
    const source = getState().tables.currentDataSource;
    const { resourceVersion } = getState().metadata;
    const upQuery = {
      type: 'bulk',
      source,
      args: upQueries,
      resource_version: resourceVersion,
    };

    const downQuery = {
      type: 'bulk',
      source,
      args: downQueries,
    };

    const migrationBody = {
      name: migrationName,
      up: upQuery.args,
      down: downQuery.args,
      datasource: source,
    };

    const currMigrationMode = getState().main.migrationMode;

    const migrateUrl = returnMigrateUrl(currMigrationMode, upQueries);

    let finalReqBody;
    if (globals.consoleMode === SERVER_CONSOLE_MODE) {
      finalReqBody = upQuery;
    } else if (globals.consoleMode === CLI_CONSOLE_MODE) {
      finalReqBody = migrationBody;
    }

    const options = {
      method: 'POST',
      credentials: globalCookiePolicy,
      headers: dataHeaders(getState),
      body: JSON.stringify(finalReqBody),
    };

    const onSuccess = data => {
      if (globals.consoleMode === CLI_CONSOLE_MODE) {
        dispatch(loadMigrationStatus());
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
    return dispatch(requestAction(migrateUrl, options)).then(
      onSuccess,
      onError
    );
  };
};

const saveRemoteSchemaPermission = (successCb, errorCb) => {
  return (dispatch, getState) => {
    const allRemoteSchemas = getRemoteSchemas(getState());

    const {
      listData: { viewRemoteSchema: currentRemoteSchemaName },
      permissions: { permissionEdit, schemaDefinition },
    } = getState().remoteSchemas;

    const currentRemoteSchema = allRemoteSchemas.find(
      rs => rs.name === currentRemoteSchemaName
    );
    const allPermissions = currentRemoteSchema?.permissions || [];

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
      dispatch(exportMetadata());
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
      dispatch(exportMetadata());
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
    const allRemoteSchemas = getRemoteSchemas(getState());
    const {
      listData: { viewRemoteSchema: currentRemoteSchemaName },
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
        return el.role === role;
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
  VIEW_REMOTE_SCHEMA,
  makeRequest,
  saveRemoteSchemaPermission,
  removeRemoteSchemaPermission,
  permRemoveMultipleRoles,
  FILTER_REMOTE_SCHEMAS,
  SET_REMOTE_SCHEMAS,
};
export default listReducer;
