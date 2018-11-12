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

import { showSuccessNotification } from '../Data/Notification';

/* Action constants */

const FETCH_RESOLVERS = '@customResolver/FETCH_RESOLVERS';
const RESOLVERS_FETCH_SUCCESS = '@customResolver/RESOLVERS_FETCH_SUCCESS';
const FILTER_RESOLVER = '@customResolver/FILTER_RESOLVER';
const RESOLVERS_FETCH_FAIL = '@customResolver/RESOLVERS_FETCH_FAIL';
const RESET = '@customResolver/RESET';

const VIEW_RESOLVER = '@customResolver/VIEW_RESOLVER';

/* */

const fetchResolvers = () => {
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
    dispatch({ type: FETCH_RESOLVERS });
    return dispatch(requestAction(url, options)).then(
      data => {
        dispatch({ type: RESOLVERS_FETCH_SUCCESS, data: data });
        return Promise.resolve();
      },
      error => {
        console.error('Failed to load triggers' + JSON.stringify(error));
        dispatch({ type: RESOLVERS_FETCH_FAIL, data: error });
        return Promise.reject();
      }
    );
  };
};

const listReducer = (state = listState, action) => {
  switch (action.type) {
    case FETCH_RESOLVERS:
      return {
        ...state,
        isRequesting: true,
        isError: false,
      };

    case RESOLVERS_FETCH_SUCCESS:
      return {
        ...state,
        resolvers: action.data,
        isRequesting: false,
        isError: false,
      };

    case RESOLVERS_FETCH_FAIL:
      return {
        ...state,
        resolvers: [],
        isRequesting: false,
        isError: action.data,
      };
    case FILTER_RESOLVER:
      return {
        ...state,
        ...action.data,
      };
    case RESET:
      return {
        ...listState,
      };
    case VIEW_RESOLVER:
      return {
        ...state,
        viewResolver: action.data,
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

export { fetchResolvers, FILTER_RESOLVER, VIEW_RESOLVER, makeRequest };
export default listReducer;
