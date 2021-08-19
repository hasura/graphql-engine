import { dataSource } from '../../../../dataSources';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import { exportMetadata } from '../../../../metadata/actions';
import {
  createFunctionPermissionQuery,
  dropFunctionPermissionQuery,
  getTrackFunctionQuery,
  getUntrackFunctionQuery,
} from '../../../../metadata/queryUtils';
import requestAction from '../../../../utils/requestAction';
import { getSchemaBaseRoute } from '../../../Common/utils/routesUtils';
import { getRunSqlQuery } from '../../../Common/utils/v1QueryUtils';
import { makeRequest } from '../../RemoteSchema/Actions';
import dataHeaders from '../Common/Headers';
import _push from '../push';
import { functionData } from './customFunctionState';

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

const SESSVAR_CUSTOM_FUNCTION_REQUEST =
  '@customFunction/SESSVAR_CUSTOM_FUNCTION_REQUEST';
const SESSVAR_CUSTOM_FUNCTION_ADD_FAIL =
  '@customFunction/SESSVAR_CUSTOM_FUNCTION_ADD_FAIL';
const SESSVAR_CUSTOM_FUNCTION_ADD_SUCCESS =
  '@customFunction/SESSVAR_CUSTOM_FUNCTION_ADD_SUCCESS';

const PERMISSION_CUSTOM_FUNCTION_SET_SUCCESS =
  '@customFunction/PERMISSION_CUSTOM_FUNCTION_SET_SUCCESS';
const PERMISSION_CUSTOM_FUNCTION_SET_FAIL =
  '@customFunction/PERMISSION_CUSTOM_FUNCTION_SET_FAIL';
const PERMISSION_CUSTOM_FUNCTION_DROP_SUCCESS =
  '@customFunction/PERMISSION_CUSTOM_FUNCTION_DROP_SUCCESS';
const PERMISSION_CUSTOM_FUNCTION_DROP_FAIL =
  '@customFunction/PERMISSION_CUSTOM_FUNCTION_DROP_FAIL';

/* Action creators */
const fetchCustomFunction = (functionName, schema, source) => {
  return (dispatch, getState) => {
    const url = Endpoints.query;
    if (!dataSource.getFunctionDefinitionSql) return;
    const fetchCustomFunctionDefinition = getRunSqlQuery(
      dataSource.getFunctionDefinitionSql(schema, functionName),
      source,
      false,
      true
    );

    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(fetchCustomFunctionDefinition),
    };
    dispatch({ type: FETCHING_INDIV_CUSTOM_FUNCTION });
    return dispatch(requestAction(url, options)).then(
      ({ result }) => {
        if (result.length > 1) {
          let funDefinition = {};
          try {
            funDefinition = JSON.parse(result[1])[0];
          } catch (err) {
            return dispatch({ type: CUSTOM_FUNCTION_FETCH_FAIL, data: err });
          }
          dispatch({
            type: CUSTOM_FUNCTION_FETCH_SUCCESS,
            data: funDefinition,
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

const deleteFunction = () => (dispatch, getState) => {
  const currentSchema = getState().tables.currentSchema;
  const { functionName, functionDefinition } = getState().functions;
  const source = getState().tables.currentDataSource;
  const upSql = dataSource.deleteFunctionSql(
    currentSchema,
    getState().functions
  );

  const sqlUpQueries = [getRunSqlQuery(upSql, source)];
  const sqlDownQueries = [];
  if (functionDefinition && functionDefinition.length > 0) {
    sqlDownQueries.push(getRunSqlQuery(functionDefinition, source));
  }

  // Apply migrations
  const migrationName = `drop_function_${currentSchema}_${functionName}`;

  const requestMsg = 'Deleting function...';
  const successMsg = 'Function deleted';
  const errorMsg = 'Deleting function failed';

  const customOnSuccess = () => {
    dispatch({ type: DELETING_CUSTOM_FUNCTION });
    dispatch(_push(getSchemaBaseRoute(currentSchema, source)));
  };
  const customOnError = () => dispatch({ type: DELETE_CUSTOM_FUNCTION_FAIL });

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

const unTrackCustomFunction = () => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    const currentDataSource = getState().tables.currentDataSource;
    const { functionName, configuration } = getState().functions;

    const migrationName = 'remove_custom_function_' + functionName;
    const payload = getUntrackFunctionQuery(
      functionName,
      currentSchema,
      currentDataSource
    );
    const downPayload = getTrackFunctionQuery(
      functionName,
      currentSchema,
      currentDataSource,
      configuration
    );

    const requestMsg = 'Deleting custom function...';
    const successMsg = 'Custom function deleted successfully';
    const errorMsg = 'Delete custom function failed';

    const customOnSuccess = () => {
      dispatch({ type: UNTRACKING_CUSTOM_FUNCTION });
      dispatch(_push(getSchemaBaseRoute(currentSchema, currentDataSource)));
      dispatch({ type: RESET });
      dispatch(exportMetadata());
    };
    const customOnError = error => {
      Promise.all([
        dispatch({ type: UNTRACK_CUSTOM_FUNCTION_FAIL, data: error }),
      ]);
    };

    return dispatch(
      makeRequest(
        [payload],
        [downPayload],
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
const updateSessVar = session_argument => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    const currentDataSource = getState().tables.currentDataSource;
    const functionName = getState().functions.functionName;
    const oldConfiguration = getState().functions.configuration;

    const migrationName = 'update_session_arg_custom_function_' + functionName;

    //untrack function first
    const untrackPayloadUp = getUntrackFunctionQuery(
      functionName,
      currentSchema,
      currentDataSource
    );
    const retrackPayloadDown = getTrackFunctionQuery(
      functionName,
      currentSchema,
      currentDataSource,
      {
        ...(oldConfiguration && oldConfiguration),
      }
    );

    // retrack with sess arg config
    const retrackPayloadUp = getTrackFunctionQuery(
      functionName,
      currentSchema,
      currentDataSource,
      {
        ...(session_argument && {
          session_argument,
        }),
      }
    );

    const untrackPayloadDown = getUntrackFunctionQuery(
      functionName,
      currentSchema,
      currentDataSource
    );

    const upQuery = {
      type: 'bulk',
      source: currentDataSource,
      args: [untrackPayloadUp, retrackPayloadUp],
    };

    const downQuery = {
      type: 'bulk',
      source: currentDataSource,
      args: [untrackPayloadDown, retrackPayloadDown],
    };

    const requestMsg = 'Updating Session argument variable...';
    const successMsg = 'Session variable argument updated successfully';
    const errorMsg = 'Updating Session argument variable failed';

    const customOnSuccess = () => {
      dispatch({ type: SESSVAR_CUSTOM_FUNCTION_REQUEST });
      dispatch(exportMetadata());
    };
    const customOnError = error => {
      dispatch({ type: SESSVAR_CUSTOM_FUNCTION_ADD_FAIL, data: error });
    };

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

const setFunctionPermission = (userRole, onSuccessCb) => (
  dispatch,
  getState
) => {
  const { currentDataSource, currentSchema } = getState().tables;
  const { functionName } = getState().functions;
  const currentFunction = { schema: currentSchema, name: functionName };

  const upQuery = createFunctionPermissionQuery(
    currentDataSource,
    currentFunction,
    userRole
  );
  const downQuery = dropFunctionPermissionQuery(
    currentDataSource,
    currentFunction,
    userRole
  );

  const migrationName = `set_permission_role_${userRole}_function_${functionName}`;
  const requestMsg = `Setting permisions for ${userRole} role on ${functionName}`;
  const successMsg = `Successfully set permissions for ${userRole}`;
  const errorMsg = `Failed to set permissions for ${userRole}`;

  const customOnSuccess = () => {
    dispatch({ type: PERMISSION_CUSTOM_FUNCTION_SET_SUCCESS });
    dispatch(exportMetadata(onSuccessCb));
  };

  const customOnError = err => {
    dispatch({ type: PERMISSION_CUSTOM_FUNCTION_SET_FAIL, data: err });
  };

  return dispatch(
    makeRequest(
      [upQuery],
      [downQuery],
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    )
  );
};

const dropFunctionPermission = (userRole, onSuccessCb) => (
  dispatch,
  getState
) => {
  const { currentDataSource, currentSchema } = getState().tables;
  const { functionName } = getState().functions;
  const currentFunction = { schema: currentSchema, name: functionName };

  const upQuery = dropFunctionPermissionQuery(
    currentDataSource,
    currentFunction,
    userRole
  );
  const downQuery = createFunctionPermissionQuery(
    currentDataSource,
    currentFunction,
    userRole
  );

  const migrationName = `drop_permission_role_${userRole}_function_${functionName}`;
  const requestMsg = `Dropping permisions for ${userRole} role on ${functionName}`;
  const successMsg = `Successfully dropped permissions for ${userRole}`;
  const errorMsg = `Failed to drop permissions for ${userRole}`;

  const customOnSuccess = () => {
    dispatch({ type: PERMISSION_CUSTOM_FUNCTION_DROP_SUCCESS });
    dispatch(exportMetadata(onSuccessCb));
  };

  const customOnError = err => {
    dispatch({ type: PERMISSION_CUSTOM_FUNCTION_DROP_FAIL, data: err });
  };

  return dispatch(
    makeRequest(
      [upQuery],
      [downQuery],
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    )
  );
};

/* Reducer */
const customFunctionReducer = (state = functionData, action) => {
  switch (action.type) {
    case RESET:
      return functionData;
    case FETCHING_INDIV_CUSTOM_FUNCTION:
      return {
        ...state,
        isFetching: true,
        isFetchError: null,
      };
    case CUSTOM_FUNCTION_FETCH_SUCCESS:
      return {
        ...state,
        functionName: action?.data?.function_name,
        functionSchema: action?.data?.function_schema || null,
        functionDefinition: action?.data?.function_definition || null,
        setOffTable: action?.data?.return_type_name || null,
        setOffTableSchema: action?.data?.return_type_schema || null,
        inputArgNames: action?.data?.input_arg_names || null,
        inputArgTypes: action?.data?.input_arg_types || null,
        isFetching: false,
        isUpdating: false,
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
    case SESSVAR_CUSTOM_FUNCTION_REQUEST:
      return {
        ...state,
        isUpdating: true,
        isError: null,
      };
    case SESSVAR_CUSTOM_FUNCTION_ADD_FAIL:
      return {
        ...state,
        isUpdating: false,
        isError: action.data,
      };
    case SESSVAR_CUSTOM_FUNCTION_ADD_SUCCESS:
      return {
        ...state,
        isUpdating: false,
        isError: null,
      };
    case PERMISSION_CUSTOM_FUNCTION_SET_SUCCESS:
      return {
        ...state,
        isPermissionSet: true,
        isError: null,
      };
    case PERMISSION_CUSTOM_FUNCTION_SET_FAIL:
      return {
        ...state,
        isPermissionSet: false,
        isError: action.data,
      };
    case PERMISSION_CUSTOM_FUNCTION_DROP_SUCCESS:
      return {
        ...state,
        isPermissionDrop: true,
        isError: null,
      };
    case PERMISSION_CUSTOM_FUNCTION_DROP_FAIL:
      return {
        ...state,
        isPermissionDrop: false,
        isError: action.data,
      };
    default:
      return state;
  }
};

export {
  deleteFunction,
  dropFunctionPermission,
  fetchCustomFunction,
  RESET,
  setFunctionPermission,
  unTrackCustomFunction,
  updateSessVar,
};
export default customFunctionReducer;
