/* Import default State */

import { functionData } from './customFunctionState';

import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';

import requestAction from '../../../../utils/requestAction';
import dataHeaders from '../Common/Headers';

import { fetchTrackedFunctions } from '../DataActions';

import _push from '../push';
import { getSchemaBaseRoute } from '../../../Common/utils/routesUtils';
import { getRunSqlQuery } from '../../../Common/utils/v1QueryUtils';
import { makeRequest } from '../../RemoteSchema/Actions';
import Migration from '../../../../utils/migration/Migration';

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

    const functionNameWithSchema =
      '"' + currentSchema + '"' + '.' + '"' + functionName + '"';

    let functionArgString = '';
    if (inputArgTypes.length > 0) {
      functionArgString += '(';
      inputArgTypes.forEach((inputArg, i) => {
        functionArgString += i > 0 ? ', ' : '';

        functionArgString +=
          '"' + inputArg.schema + '"' + '.' + '"' + inputArg.name + '"';
      });
      functionArgString += ')';
    }

    const sqlDropFunction =
      'DROP FUNCTION ' + functionNameWithSchema + functionArgString;

    const migration = new Migration();

    if (functionDefinition && functionDefinition.length > 0) {
      migration.add(
        getRunSqlQuery(sqlDropFunction),
        getRunSqlQuery(functionDefinition)
      );
    } else {
      migration.add(getRunSqlQuery(sqlDropFunction)); // TODO Down queries
    }

    // Apply migrations
    const migrationName = 'drop_function_' + currentSchema + '_' + functionName;

    const requestMsg = 'Deleting function...';
    const successMsg = 'Function deleted';
    const errorMsg = 'Deleting function failed';

    const customOnSuccess = () => {
      dispatch(_push(getSchemaBaseRoute(currentSchema)));
    };
    const customOnError = () => {
      dispatch({ type: DELETE_CUSTOM_FUNCTION_FAIL });
    };

    dispatch({ type: DELETING_CUSTOM_FUNCTION });
    return dispatch(
      makeRequest(
        migration.upMigration,
        migration.downMigration,
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

    const migration = new Migration();
    migration.add(payload, downPayload);

    const requestMsg = 'Deleting custom function...';
    const successMsg = 'Custom function deleted successfully';
    const errorMsg = 'Delete custom function failed';

    const customOnSuccess = () => {
      dispatch(_push(getSchemaBaseRoute(currentSchema)));
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
        migration.upMigration,
        migration.downMigration,
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
