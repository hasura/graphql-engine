/* */
import { listState } from './state';
/* */

import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import requestAction from '../../../utils/requestAction';
import dataHeaders from '../Data/Common/Headers';
import globals from '../../../Globals';
import returnMigrateUrl from '../Data/Common/getMigrateUrl';
import { SERVER_CONSOLE_MODE } from '../../../constants';
import { loadMigrationStatus } from '../../Main/Actions';
import { handleMigrationErrors } from '../EventTrigger/EventActions';

import { showSuccessNotification } from '../Common/Notification';
import { filterInconsistentMetadataObjects } from '../Metadata/utils';

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
          columns: ['*'],
          order_by: [{ column: 'name', type: 'asc', nulls: 'last' }],
        },
      }),
    };
    dispatch({ type: FETCH_REMOTE_SCHEMAS });
    return dispatch(requestAction(url, options)).then(
      data => {
        let consistentRemoteSchemas = data;
        const { inconsistentObjects } = getState().metadata;

        if (inconsistentObjects.length > 0) {
          consistentRemoteSchemas = filterInconsistentMetadataObjects(
            data,
            inconsistentObjects,
            'remote_schemas'
          );
        }

        dispatch({
          type: REMOTE_SCHEMAS_FETCH_SUCCESS,
          data: consistentRemoteSchemas,
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

/* makeRequest function to identify what the current mode is and send normal query or a migration call */
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
    } else if (globals.consoleMode === 'cli') {
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
      if (globals.consoleMode === 'cli') {
        dispatch(loadMigrationStatus()); // don't call for server mode
      }
      // dispatch(loadTriggers());
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
/* */

export {
  fetchRemoteSchemas,
  FILTER_REMOTE_SCHEMAS,
  VIEW_REMOTE_SCHEMA,
  makeRequest,
  setConsistentRemoteSchemas,
};
export default listReducer;
