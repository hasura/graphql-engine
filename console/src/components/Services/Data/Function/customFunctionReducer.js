/* Import default State */

import { functionData } from './customFunctionState';

import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';

import requestAction from '../../../../utils/requestAction';
import dataHeaders from '../Common/Headers';

import globals from '../../../../Globals';

import returnMigrateUrl from '../Common/getMigrateUrl';
import { SERVER_CONSOLE_MODE } from '../../../../constants';
import { loadMigrationStatus } from '../../../Main/Actions';
import { handleMigrationErrors } from '../../EventTrigger/EventActions';

import { showSuccessNotification } from '../../Common/Notification';
// import { push } from 'react-router-redux';

import { fetchTrackedFunctions } from '../DataActions';

import _push from '../push';

/* Constants */

const RESET = '@customFunction/RESET';
const FETCHING_INDIV_CUSTOM_FUNCTION =
  '@customFunction/FETCHING_INDIV_CUSTOM_FUNCTION';
const CUSTOM_FUNCTION_FETCH_SUCCESS =
  '@customFunction/CUSTOM_FUNCTION_FETCH_SUCCESS';
const CUSTOM_FUNCTION_FETCH_FAIL = '@customFunction/CUSTOM_FUNCTION_FETCH_FAIL';
const DELETING_CUSTOM_FUNCTION = '@customFunction/DELETING_CUSTOM_FUNCTION';
const DELETE_CUSTOM_FUNCTION_FAIL =
  '@customFunction/DELETE_CUSTOM_FUNCTION_FAIL';

const UNTRACKING_CUSTOM_FUNCTION = '@customFunction/UNTRACKING_CUSTOM_FUNCTION';
const UNTRACK_CUSTOM_FUNCTION_FAIL =
  '@customFunction/UNTRACK_CUSTOM_FUNCTION_FAIL';

/* */

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

/* Action creators */
const fetchCustomFunction = (functionName, schema) => {
  return (dispatch, getState) => {
    const url = Endpoints.getSchema;
    const fetchCustomFunctionQuery = {
      type: 'select',
      args: {
        table: {
          name: 'hdb_function',
          schema: 'hdb_catalog',
        },
        columns: ['*'],
        where: {
          function_schema: schema,
          function_name: functionName,
        },
      },
    };
    const fetchCustomFunctionDefinition = {
      type: 'select',
      args: {
        table: {
          name: 'hdb_function_agg',
          schema: 'hdb_catalog',
        },
        columns: ['*'],
        where: {
          function_schema: schema,
          function_name: functionName,
        },
      },
    };

    const bulkQuery = {
      type: 'bulk',
      args: [fetchCustomFunctionQuery, fetchCustomFunctionDefinition],
    };
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(bulkQuery),
    };
    dispatch({ type: FETCHING_INDIV_CUSTOM_FUNCTION });
    return dispatch(requestAction(url, options)).then(
      data => {
        if (data[0].length > 0 && data[1].length > 0) {
          dispatch({
            type: CUSTOM_FUNCTION_FETCH_SUCCESS,
            data: [[...data[0]], [...data[1]]],
          });
          return Promise.resolve();
        }
      },
      error => {
        console.error('Failed to fetch function' + JSON.stringify(error));
        return dispatch({ type: CUSTOM_FUNCTION_FETCH_FAIL, data: error });
      }
    );
  };
};

const deleteFunctionSql = () => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    const {
      functionName,
      functionDefinition,
      inputArgTypes,
    } = getState().functions;
    let functionWSchemaName =
      '"' + currentSchema + '"' + '.' + '"' + functionName + '"';

    if (inputArgTypes.length > 0) {
      let functionString = '(';
      inputArgTypes.forEach((i, index) => {
        functionString +=
          i + ' ' + (index === inputArgTypes.length - 1 ? ')' : ',');
      });
      functionWSchemaName += functionString;
    }

    const sqlDropFunction = 'DROP FUNCTION ' + functionWSchemaName;

    const sqlUpQueries = [
      {
        type: 'run_sql',
        args: { sql: sqlDropFunction },
      },
    ];
    const sqlDownQueries = [];
    if (functionDefinition && functionDefinition.length > 0) {
      sqlDownQueries.push({
        type: 'run_sql',
        args: { sql: functionDefinition },
      });
    }

    // Apply migrations
    const migrationName = 'drop_function_' + currentSchema + '_' + functionName;

    const requestMsg = 'Deleting function...';
    const successMsg = 'Function deleted';
    const errorMsg = 'Deleting function failed';

    const customOnSuccess = () => {
      dispatch(_push(`/schema/${currentSchema}`));
    };
    const customOnError = () => {
      dispatch({ type: DELETE_CUSTOM_FUNCTION_FAIL });
    };

    dispatch({ type: DELETING_CUSTOM_FUNCTION });
    return dispatch(
      makeRequest(
        sqlUpQueries,
        sqlDownQueries,
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

const unTrackCustomFunction = () => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    const functionName = getState().functions.functionName;
    // const url = Endpoints.getSchema;
    /*
    const customFunctionObj = {
      function_name: functionName,
    };
    */
    const migrationName = 'remove_custom_function_' + functionName;
    const payload = {
      type: 'untrack_function',
      args: {
        name: functionName,
        schema: currentSchema,
      },
    };
    const downPayload = {
      type: 'track_function',
      args: {
        name: functionName,
        schema: currentSchema,
      },
    };

    const upQueryArgs = [];
    upQueryArgs.push(payload);
    const downQueryArgs = [];
    downQueryArgs.push(downPayload);
    const upQuery = {
      type: 'bulk',
      args: upQueryArgs,
    };
    const downQuery = {
      type: 'bulk',
      args: downQueryArgs,
    };
    const requestMsg = 'Deleting custom function...';
    const successMsg = 'Custom function deleted successfully';
    const errorMsg = 'Delete custom function failed';

    const customOnSuccess = () => {
      dispatch(_push(`/schema/${currentSchema}`));
      dispatch({ type: RESET });
      dispatch(fetchTrackedFunctions());
    };
    const customOnError = error => {
      Promise.all([
        dispatch({ type: UNTRACK_CUSTOM_FUNCTION_FAIL, data: error }),
      ]);
    };

    dispatch({ type: UNTRACKING_CUSTOM_FUNCTION });
    return dispatch(
      makeRequest(
        upQuery.args,
        downQuery.args,
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

/* */

/* Reducer */

const customFunctionReducer = (state = functionData, action) => {
  switch (action.type) {
    case RESET:
      return {
        ...functionData,
      };
    case FETCHING_INDIV_CUSTOM_FUNCTION:
      return {
        ...state,
        isFetching: true,
        isFetchError: null,
      };
    case CUSTOM_FUNCTION_FETCH_SUCCESS:
      return {
        ...state,
        functionName: action.data[0][0].function_name,
        functionSchema: action.data[0][0].function_schema || null,
        functionDefinition: action.data[1][0].function_definition || null,
        setOffTable: action.data[1][0].return_type_name || null,
        setOffTableSchema: action.data[1][0].return_type_schema || null,
        inputArgNames: action.data[1][0].input_arg_names || null,
        inputArgTypes: action.data[1][0].input_arg_types || null,
        isFetching: false,
        isFetchError: null,
      };
    case CUSTOM_FUNCTION_FETCH_FAIL:
      return {
        ...state,
        isFetching: false,
        isFetchError: action.data,
      };
    case DELETE_CUSTOM_FUNCTION_FAIL:
      return {
        ...state,
        isDeleting: false,
        isError: action.data,
      };
    case DELETING_CUSTOM_FUNCTION:
      return {
        ...state,
        isDeleting: true,
        isError: null,
      };

    case UNTRACK_CUSTOM_FUNCTION_FAIL:
      return {
        ...state,
        isUntracking: false,
        isError: action.data,
      };
    case UNTRACKING_CUSTOM_FUNCTION:
      return {
        ...state,
        isUntracking: true,
        isError: null,
      };
    default:
      return {
        ...state,
      };
  }
};

/* End of it */

export { RESET, fetchCustomFunction, deleteFunctionSql, unTrackCustomFunction };
export default customFunctionReducer;
