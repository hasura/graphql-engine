import sanitize from 'sanitize-filename';
import { push } from 'react-router-redux';

import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import requestAction from '../../../utils/requestAction';
import defaultState from './DataState';
import insertReducer from './TableInsertItem/InsertActions';
import viewReducer from './TableBrowseRows/ViewActions';
import editReducer from './TableBrowseRows/EditActions';
import modifyReducer from './TableCommon/TableReducer';
import { getAllUnTrackedRelations } from './TableRelationships/Actions';
import {
  showErrorNotification,
  showSuccessNotification,
} from '../Common/Notification';
import dataHeaders from './Common/Headers';
import { loadMigrationStatus } from '../../Main/Actions';
import returnMigrateUrl from './Common/getMigrateUrl';
import {
  filterInconsistentMetadata,
  loadInconsistentObjects,
} from '../Metadata/Actions';
import globals from '../../../Globals';

import {
  fetchTrackedTableReferencedFkQuery,
  fetchTrackedTableFkQuery,
  fetchTableListQuery,
  fetchTrackedTableListQuery,
  mergeLoadSchemaData,
} from './utils';
import { fetchColumnTypesQuery } from './utils';

import { SERVER_CONSOLE_MODE } from '../../../constants';

const SET_TABLE = 'Data/SET_TABLE';
const LOAD_FUNCTIONS = 'Data/LOAD_FUNCTIONS';
const LOAD_NON_TRACKABLE_FUNCTIONS = 'Data/LOAD_NON_TRACKABLE_FUNCTIONS';
const LOAD_TRACKED_FUNCTIONS = 'Data/LOAD_TRACKED_FUNCTIONS';
const LOAD_SCHEMA = 'Data/LOAD_SCHEMA';
const LOAD_UNTRACKED_RELATIONS = 'Data/LOAD_UNTRACKED_RELATIONS';
const FETCH_SCHEMA_LIST = 'Data/FETCH_SCHEMA_LIST';
const UPDATE_CURRENT_SCHEMA = 'Data/UPDATE_CURRENT_SCHEMA';
const ADMIN_SECRET_ERROR = 'Data/ADMIN_SECRET_ERROR';
const UPDATE_DATA_HEADERS = 'Data/UPDATE_DATA_HEADERS';
const UPDATE_REMOTE_SCHEMA_MANUAL_REL = 'Data/UPDATE_SCHEMA_MANUAL_REL';
const SET_CONSISTENT_SCHEMA = 'Data/SET_CONSISTENT_SCHEMA';
const SET_CONSISTENT_FUNCTIONS = 'Data/SET_CONSISTENT_FUNCTIONS';

const FETCH_COLUMN_TYPE_LIST = 'Data/FETCH_COLUMN_TYPE_LIST';
const FETCH_COLUMN_TYPE_LIST_FAIL = 'Data/FETCH_COLUMN_TYPE_LIST_FAIL';
const RESET_COLUMN_TYPE_LIST = 'Data/RESET_COLUMN_TYPE_LIST';

const MAKE_REQUEST = 'ModifyTable/MAKE_REQUEST';
const REQUEST_SUCCESS = 'ModifyTable/REQUEST_SUCCESS';
const REQUEST_ERROR = 'ModifyTable/REQUEST_ERROR';

const initQueries = {
  schemaList: {
    type: 'select',
    args: {
      table: {
        name: 'schemata',
        schema: 'information_schema',
      },
      columns: ['schema_name'],
      order_by: [{ column: 'schema_name', type: 'asc', nulls: 'last' }],
      where: {
        schema_name: {
          $nin: [
            'information_schema',
            'pg_catalog',
            'hdb_catalog',
            'hdb_views',
          ],
        },
      },
    },
  },
  loadTrackedFunctions: {
    type: 'select',
    args: {
      table: {
        name: 'hdb_function',
        schema: 'hdb_catalog',
      },
      columns: ['function_name', 'function_schema', 'is_system_defined'],
      where: {
        function_schema: '',
      },
    },
  },
  loadTrackableFunctions: {
    type: 'select',
    args: {
      table: {
        name: 'hdb_function_agg',
        schema: 'hdb_catalog',
      },
      columns: [
        'function_name',
        'function_schema',
        'has_variadic',
        'function_type',
        'function_definition',
        'return_type_schema',
        'return_type_name',
        'return_type_type',
        'returns_set',
        {
          name: 'return_table_info',
          columns: ['table_schema', 'table_name'],
        },
      ],
      where: {
        function_schema: '',
        has_variadic: false,
        returns_set: true,
        return_type_type: {
          $ilike: '%composite%',
        },
        $or: [
          {
            function_type: {
              $ilike: '%stable%',
            },
          },
          {
            function_type: {
              $ilike: '%immutable%',
            },
          },
        ],
      },
    },
  },
  loadNonTrackableFunctions: {
    type: 'select',
    args: {
      table: {
        name: 'hdb_function_agg',
        schema: 'hdb_catalog',
      },
      columns: [
        'function_name',
        'function_schema',
        'has_variadic',
        'function_type',
        'function_definition',
        'return_type_schema',
        'return_type_name',
        'return_type_type',
        'returns_set',
      ],
      where: {
        // TODO: set correct where
        function_schema: '',
        has_variadic: false,
        returns_set: true,
        return_type_type: {
          $ilike: '%composite%',
        },
        function_type: {
          $ilike: '%volatile%',
        },
      },
    },
  },
};

const fetchTrackedFunctions = () => {
  return (dispatch, getState) => {
    const url = Endpoints.getSchema;

    const currentSchema = getState().tables.currentSchema;

    const body = initQueries.loadTrackedFunctions;
    body.args.where.function_schema = currentSchema;

    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(body),
    };

    return dispatch(requestAction(url, options)).then(
      data => {
        let consistentFunctions = data;
        const { inconsistentObjects } = getState().metadata;

        if (inconsistentObjects.length > 0) {
          consistentFunctions = filterInconsistentMetadata(
            data,
            inconsistentObjects,
            'functions'
          );
        }

        dispatch({ type: LOAD_TRACKED_FUNCTIONS, data: consistentFunctions });
      },
      error => {
        console.error('Failed to load schema ' + JSON.stringify(error));
      }
    );
  };
};

const setUntrackedRelations = () => (dispatch, getState) => {
  const untrackedRelations = getAllUnTrackedRelations(
    getState().tables.allSchemas,
    getState().tables.currentSchema
  ).bulkRelTrack;

  dispatch({
    type: LOAD_UNTRACKED_RELATIONS,
    untrackedRelations,
  });
};

const loadSchema = configOptions => {
  return (dispatch, getState) => {
    const url = Endpoints.getSchema;

    let allSchemas = getState().tables.allSchemas;

    if (
      !configOptions ||
      ((!configOptions.schemas || configOptions.schemas.length === 0) &&
        (!configOptions.tables || configOptions.tables.length === 0))
    ) {
      configOptions = {
        schemas: [getState().tables.currentSchema],
      };
    }

    if (configOptions) {
      if (configOptions.schemas) {
        allSchemas = allSchemas.filter(
          schemaInfo =>
            !configOptions.schemas.some(
              item => item === schemaInfo.table_schema
            )
        );
      }

      if (configOptions.tables) {
        allSchemas = allSchemas.filter(
          schemaInfo =>
            !configOptions.tables.some(
              item =>
                item.table_schema === schemaInfo.table_schema &&
                item.table_name === schemaInfo.table_name
            )
        );
      }
    }

    const body = {
      type: 'bulk',
      args: [
        fetchTableListQuery(configOptions),
        fetchTrackedTableListQuery(configOptions), // v1/query
        fetchTrackedTableFkQuery(configOptions),
        fetchTrackedTableReferencedFkQuery(configOptions),
      ],
    };

    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(body),
    };

    return dispatch(requestAction(url, options)).then(
      data => {
        const mergedData = mergeLoadSchemaData(
          JSON.parse(data[0].result[1]),
          data[1],
          JSON.parse(data[2].result[1]),
          JSON.parse(data[3].result[1])
        );

        const { inconsistentObjects } = getState().metadata;

        const maybeInconsistentSchemas = allSchemas.concat(mergedData);

        let consistentSchemas;
        if (inconsistentObjects.length > 0) {
          consistentSchemas = filterInconsistentMetadata(
            maybeInconsistentSchemas,
            inconsistentObjects,
            'tables'
          );
        }

        dispatch({
          type: LOAD_SCHEMA,
          allSchemas: consistentSchemas || maybeInconsistentSchemas,
        });

        dispatch(loadInconsistentObjects());
      },
      error => {
        console.error('Failed to load schema ' + JSON.stringify(error));
      }
    );
  };
};

const updateSchemaInfo = options => dispatch => {
  return dispatch(loadSchema(options)).then(() => {
    dispatch(setUntrackedRelations());
  });
};

const setConsistentSchema = data => ({
  type: SET_CONSISTENT_SCHEMA,
  data,
});

const setConsistentFunctions = data => ({
  type: SET_CONSISTENT_FUNCTIONS,
  data,
});

const fetchDataInit = () => (dispatch, getState) => {
  const url = Endpoints.getSchema;

  const body = {
    type: 'bulk',
    args: [initQueries.schemaList],
  };

  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(body),
  };

  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: FETCH_SCHEMA_LIST, schemaList: data[0] });
      dispatch(updateSchemaInfo());
    },
    error => {
      console.error('Failed to fetch schema ' + JSON.stringify(error));
    }
  );
};

const fetchFunctionInit = () => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const body = {
    type: 'bulk',
    args: [
      initQueries.loadTrackableFunctions,
      initQueries.loadNonTrackableFunctions,
      initQueries.loadTrackedFunctions,
    ],
  };

  // set schema in queries
  const currentSchema = getState().tables.currentSchema;
  body.args[0].args.where.function_schema = currentSchema;
  body.args[1].args.where.function_schema = currentSchema;
  body.args[2].args.where.function_schema = currentSchema;

  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(body),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: LOAD_FUNCTIONS, data: data[0] });
      dispatch({ type: LOAD_NON_TRACKABLE_FUNCTIONS, data: data[1] });
      let consistentFunctions = data[2];
      const { inconsistentObjects } = getState().metadata;
      if (inconsistentObjects.length > 0) {
        consistentFunctions = filterInconsistentMetadata(
          consistentFunctions,
          inconsistentObjects,
          'functions'
        );
      }
      dispatch({ type: LOAD_TRACKED_FUNCTIONS, data: consistentFunctions });
    },
    error => {
      console.error('Failed to fetch schema ' + JSON.stringify(error));
    }
  );
};

const updateCurrentSchema = schemaName => dispatch => {
  dispatch(push(`${globals.urlPrefix}/data/schema/${schemaName}`));

  Promise.all([
    dispatch({ type: UPDATE_CURRENT_SCHEMA, currentSchema: schemaName }),
    dispatch(setUntrackedRelations()),
    dispatch(fetchFunctionInit()),
    dispatch(updateSchemaInfo()),
  ]);
};

/* ************ action creators *********************** */
const fetchSchemaList = () => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(initQueries.schemaList),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: FETCH_SCHEMA_LIST, schemaList: data });
    },
    error => {
      console.error('Failed to fetch schema ' + JSON.stringify(error));
    }
  );
};

const setTable = tableName => ({ type: SET_TABLE, tableName });

/* **********Shared functions between table actions********* */

const handleMigrationErrors = (title, errorMsg) => dispatch => {
  const requestMsg = title;
  if (globals.consoleMode === SERVER_CONSOLE_MODE) {
    // handle errors for run_sql based workflow
    dispatch(showErrorNotification(title, errorMsg.code, requestMsg, errorMsg));
  } else if (errorMsg.code === 'migration_failed') {
    dispatch(
      showErrorNotification(title, 'Migration Failed', requestMsg, errorMsg)
    );
  } else if (errorMsg.code === 'data_api_error') {
    const parsedErrorMsg = errorMsg;
    parsedErrorMsg.message = JSON.parse(errorMsg.message);
    dispatch(
      showErrorNotification(
        title,
        parsedErrorMsg.message.error,
        requestMsg,
        parsedErrorMsg
      )
    );
  } else {
    // any other unhandled codes
    const parsedErrorMsg = errorMsg;
    parsedErrorMsg.message = JSON.parse(errorMsg.message);
    dispatch(
      showErrorNotification(title, errorMsg.code, requestMsg, parsedErrorMsg)
    );
  }
  // dispatch(showErrorNotification(msg, firstDisplay, request, response));
};

const makeMigrationCall = (
  dispatch,
  getState,
  upQueries,
  downQueries,
  migrationName,
  customOnSuccess,
  customOnError,
  requestMsg,
  successMsg,
  errorMsg,
  shouldSkipSchemaReload
) => {
  const upQuery = {
    type: 'bulk',
    args: upQueries,
  };

  const downQuery = {
    type: 'bulk',
    args: downQueries,
  };

  const migrationBody = {
    name: sanitize(migrationName),
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

  const onSuccess = () => {
    if (!shouldSkipSchemaReload) {
      if (globals.consoleMode === 'cli') {
        dispatch(loadMigrationStatus()); // don't call for server mode
      }
      dispatch(updateSchemaInfo());
    }
    if (successMsg) {
      dispatch(showSuccessNotification(successMsg));
    }
    customOnSuccess();
  };

  const onError = err => {
    customOnError(err);
    dispatch(handleMigrationErrors(errorMsg, err));
  };

  dispatch({ type: MAKE_REQUEST });
  dispatch(showSuccessNotification(requestMsg));
  dispatch(requestAction(url, options, REQUEST_SUCCESS, REQUEST_ERROR)).then(
    onSuccess,
    onError
  );
};

const fetchColumnTypes = () => {
  return (dispatch, getState) => {
    const url = Endpoints.getSchema;
    const reqQuery = {
      type: 'run_sql',
      args: {
        sql: fetchColumnTypesQuery,
      },
    };
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(reqQuery),
    };
    return dispatch(requestAction(url, options)).then(
      data => {
        return dispatch({
          type: FETCH_COLUMN_TYPE_LIST,
          data: data.result.slice(1),
        });
      },
      error => {
        dispatch(
          showErrorNotification(
            'Error fetching column types',
            'Kindly reach out to us in case you face this issue again',
            error,
            error
          )
        );
        return dispatch({
          type: FETCH_COLUMN_TYPE_LIST_FAIL,
          data: error,
        });
      }
    );
  };
};

/* ******************************************************* */
const dataReducer = (state = defaultState, action) => {
  // eslint-disable-line no-unused-vars
  if (action.type.indexOf('ViewTable/') === 0) {
    return {
      ...state,
      view: viewReducer(
        state.currentTable,
        state.currentSchema,
        state.allSchemas,
        state.view,
        action
      ),
    };
  }
  if (action.type.indexOf('ModifyTable/') === 0) {
    return {
      ...state,
      modify: modifyReducer(
        state.currentTable,
        state.allSchemas,
        state.modify,
        action
      ),
    };
  }
  if (action.type.indexOf('InsertItem/') === 0) {
    return {
      ...state,
      insert: insertReducer(state.currentTable, state.insert, action),
    };
  }
  if (action.type.indexOf('EditItem/') === 0) {
    return {
      ...state,
      update: editReducer(state.currentTable, state.update, action),
    };
  }
  switch (action.type) {
    case LOAD_FUNCTIONS:
      return {
        ...state,
        postgresFunctions: action.data,
      };
    case LOAD_NON_TRACKABLE_FUNCTIONS:
      return {
        ...state,
        nonTrackablePostgresFunctions: action.data,
      };
    case LOAD_TRACKED_FUNCTIONS:
      return {
        ...state,
        trackedFunctions: action.data,
      };
    case LOAD_SCHEMA:
      // remove duplicates
      const result = action.allSchemas.reduce((unique, o) => {
        if (
          !unique.some(
            obj =>
              obj.table_name === o.table_name &&
              obj.table_schema === o.table_schema
          )
        ) {
          unique.push(o);
        }
        return unique;
      }, []);
      return {
        ...state,
        allSchemas: result,
      };
    case LOAD_UNTRACKED_RELATIONS:
      return {
        ...state,
        untrackedRelations: action.untrackedRelations,
      };
    case SET_TABLE:
      return { ...state, currentTable: action.tableName };
    case FETCH_SCHEMA_LIST:
      return { ...state, schemaList: action.schemaList };
    case SET_CONSISTENT_SCHEMA:
      return { ...state, allSchemas: action.data, listingSchemas: action.data };
    case SET_CONSISTENT_FUNCTIONS:
      return {
        ...state,
        trackedFunctions: action.data,
      };
    case UPDATE_CURRENT_SCHEMA:
      return { ...state, currentSchema: action.currentSchema };
    case ADMIN_SECRET_ERROR:
      return { ...state, adminSecretError: action.data };
    case UPDATE_DATA_HEADERS:
      return { ...state, dataHeaders: action.data };
    case UPDATE_REMOTE_SCHEMA_MANUAL_REL:
      return {
        ...state,
        modify: {
          ...state.modify,
          manualRelAdd: {
            ...state.modify.manualRelAdd,
            rSchema: action.data,
          },
        },
      };
    case FETCH_COLUMN_TYPE_LIST:
      return {
        ...state,
        columnDataTypes: action.data,
        columnDataTypeFetchErr: defaultState.columnDataTypeFetchErr,
      };

    case FETCH_COLUMN_TYPE_LIST_FAIL:
      return {
        ...state,
        columnDataTypes: [],
        columnDataTypeFetchErr: action.data,
      };
    case RESET_COLUMN_TYPE_LIST:
      return {
        ...state,
        columnDataTypes: [...defaultState.columnDataTypes],
        columnDataTypeFetchErr: defaultState.columnDataTypeFetchErr,
      };
    default:
      return state;
  }
};

export default dataReducer;
export {
  MAKE_REQUEST,
  REQUEST_SUCCESS,
  REQUEST_ERROR,
  setTable,
  updateSchemaInfo,
  handleMigrationErrors,
  makeMigrationCall,
  LOAD_UNTRACKED_RELATIONS,
  UPDATE_CURRENT_SCHEMA,
  fetchSchemaList,
  fetchDataInit,
  fetchFunctionInit,
  updateCurrentSchema,
  ADMIN_SECRET_ERROR,
  UPDATE_DATA_HEADERS,
  UPDATE_REMOTE_SCHEMA_MANUAL_REL,
  fetchTrackedFunctions,
  initQueries,
  LOAD_SCHEMA,
  setConsistentSchema,
  setConsistentFunctions,
  fetchColumnTypes,
  RESET_COLUMN_TYPE_LIST,
  setUntrackedRelations,
};
