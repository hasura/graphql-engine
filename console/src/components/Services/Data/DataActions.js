import React from 'react';
import sanitize from 'sanitize-filename';

import { getSchemaBaseRoute } from '../../Common/utils/routesUtils';
import { getRunSqlQuery } from '../../Common/utils/v1QueryUtils';
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
  getErrorMessage,
  showNotification,
} from '../Common/Notification';
import dataHeaders from './Common/Headers';
import { loadMigrationStatus } from '../../Main/Actions';
import returnMigrateUrl from './Common/getMigrateUrl';
import { loadInconsistentObjects } from '../Settings/Actions';
import { filterInconsistentMetadataObjects } from '../Settings/utils';
import globals from '../../../Globals';

import {
  fetchTrackedTableReferencedFkQuery,
  fetchTrackedTableFkQuery,
  fetchTableListQuery,
  fetchTrackedTableListQuery,
  fetchTrackedTableRemoteRelationshipQuery,
  mergeLoadSchemaData,
  cascadeUpQueries,
  getDependencyError,
} from './utils';

import _push from './push';
import { getFetchAllRolesQuery } from '../../Common/utils/v1QueryUtils';

import { fetchColumnTypesQuery, fetchColumnDefaultFunctions } from './utils';

import { fetchColumnCastsQuery, convertArrayToJson } from './TableModify/utils';

import { CLI_CONSOLE_MODE, SERVER_CONSOLE_MODE } from '../../../constants';
import { getDownQueryComments } from '../../../utils/migration/utils';
import { isEmpty } from '../../Common/utils/jsUtils';

const SET_TABLE = 'Data/SET_TABLE';
const LOAD_FUNCTIONS = 'Data/LOAD_FUNCTIONS';
const LOAD_NON_TRACKABLE_FUNCTIONS = 'Data/LOAD_NON_TRACKABLE_FUNCTIONS';
const LOAD_TRACKED_FUNCTIONS = 'Data/LOAD_TRACKED_FUNCTIONS';
const LOAD_SCHEMA = 'Data/LOAD_SCHEMA';
const LOAD_UNTRACKED_RELATIONS = 'Data/LOAD_UNTRACKED_RELATIONS';
const FETCH_SCHEMA_LIST = 'Data/FETCH_SCHEMA_LIST';
const UPDATE_CURRENT_SCHEMA = 'Data/UPDATE_CURRENT_SCHEMA';
const ADMIN_SECRET_ERROR = 'Data/ADMIN_SECRET_ERROR';
const UPDATE_REMOTE_SCHEMA_MANUAL_REL = 'Data/UPDATE_SCHEMA_MANUAL_REL';
const SET_CONSISTENT_SCHEMA = 'Data/SET_CONSISTENT_SCHEMA';
const SET_CONSISTENT_FUNCTIONS = 'Data/SET_CONSISTENT_FUNCTIONS';

const UPDATE_DATA_HEADERS = 'Data/UPDATE_DATA_HEADERS';

const FETCH_COLUMN_TYPE_INFO = 'Data/FETCH_COLUMN_TYPE_INFO';
const FETCH_COLUMN_TYPE_INFO_FAIL = 'Data/FETCH_COLUMN_TYPE_INFO_FAIL';
const RESET_COLUMN_TYPE_INFO = 'Data/RESET_COLUMN_TYPE_INFO';

const MAKE_REQUEST = 'ModifyTable/MAKE_REQUEST';
const REQUEST_SUCCESS = 'ModifyTable/REQUEST_SUCCESS';
const REQUEST_ERROR = 'ModifyTable/REQUEST_ERROR';

const SET_ADDITIONAL_COLUMNS_INFO = 'Data/SET_ADDITIONAL_COLUMNS_INFO';

export const SET_ALL_ROLES = 'Data/SET_ALL_ROLES';
export const setAllRoles = roles => ({
  type: SET_ALL_ROLES,
  roles,
});

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
      order_by: [{ column: 'function_name', type: 'asc', nulls: 'last' }],
      where: {
        function_schema: '', // needs to be set later
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
      order_by: [{ column: 'function_name', type: 'asc', nulls: 'last' }],
      where: {
        function_schema: '', // needs to be set later
        has_variadic: false,
        returns_set: true,
        return_type_type: 'c', // COMPOSITE type
        return_table_info: {},
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
        {
          name: 'return_table_info',
          columns: ['table_schema', 'table_name'],
        },
      ],
      order_by: [{ column: 'function_name', type: 'asc', nulls: 'last' }],
      where: {
        function_schema: '', // needs to be set later
        $not: {
          has_variadic: false,
          returns_set: true,
          return_type_type: 'c', // COMPOSITE type
          return_table_info: {},
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
  },
};

export const mergeRemoteRelationshipsWithSchema = (
  remoteRelationships,
  table
) => {
  return (dispatch, getState) => {
    const { allSchemas } = getState().tables;
    const t = allSchemas.find(s => {
      return s.table_name === table.name && s.table_schema === table.schema;
    });
    if (!t) return;
    const newAllSchemas = allSchemas.filter(
      s => !(s.table_name === table.name && s.table_schema === table.schema)
    );
    newAllSchemas.push({
      ...t,
      remote_relationships: remoteRelationships,
    });
    dispatch({
      type: LOAD_SCHEMA,
      allSchemas: newAllSchemas,
    });
  };
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
          consistentFunctions = filterInconsistentMetadataObjects(
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
        fetchTrackedTableRemoteRelationshipQuery(configOptions),
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
        const tableList = JSON.parse(data[0].result[1]);
        const fkList = JSON.parse(data[2].result[1]);
        const refFkList = JSON.parse(data[3].result[1]);
        const remoteRelationships = data[4];

        const mergedData = mergeLoadSchemaData(
          tableList,
          data[1],
          fkList,
          refFkList,
          remoteRelationships
        );

        const { inconsistentObjects } = getState().metadata;

        const maybeInconsistentSchemas = allSchemas.concat(mergedData);

        let consistentSchemas;
        if (inconsistentObjects.length > 0) {
          consistentSchemas = filterInconsistentMetadataObjects(
            maybeInconsistentSchemas,
            inconsistentObjects,
            'tables'
          );
        }

        dispatch({
          type: LOAD_SCHEMA,
          allSchemas: consistentSchemas || maybeInconsistentSchemas,
        });

        dispatch(loadInconsistentObjects({ shouldReloadMetadata: false }));
      },
      error => {
        console.error('loadSchema error: ' + JSON.stringify(error));
        dispatch(
          showErrorNotification('DB schema loading failed', null, error)
        );
      }
    );
  };
};

const fetchAdditionalColumnsInfo = () => (dispatch, getState) => {
  const schemaName = getState().tables.currentSchema;
  const query = {
    type: 'select',
    args: {
      table: {
        name: 'columns',
        schema: 'information_schema',
      },
      columns: [
        'column_name',
        'table_name',
        'is_generated',
        'is_identity',
        'identity_generation',
      ],
      where: {
        table_schema: {
          $eq: schemaName,
        },
      },
    },
  };
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(query),
  };

  return dispatch(requestAction(Endpoints.query, options)).then(
    result => {
      if (result) {
        let columnsInfo = {};
        result
          .filter(
            info => info.is_generated !== 'NEVER' || info.is_identity !== 'NO'
          )
          .forEach(
            ({
              column_name,
              table_name,
              is_generated,
              is_identity,
              identity_generation,
            }) => {
              columnsInfo = {
                ...columnsInfo,
                [table_name]: {
                  ...columnsInfo[table_name],
                  [column_name]: {
                    is_generated,
                    is_identity,
                    identity_generation,
                  },
                },
              };
            }
          );
        dispatch({
          type: SET_ADDITIONAL_COLUMNS_INFO,
          data: columnsInfo,
        });
      }
    },
    error => {
      console.error(
        'Failed to load additional columns information ' + JSON.stringify(error)
      );
    }
  );
};

const updateSchemaInfo = options => dispatch => {
  return dispatch(loadSchema(options)).then(() => {
    dispatch(fetchAdditionalColumnsInfo());
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

const fetchFunctionInit = (schema = null) => (dispatch, getState) => {
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
  const fnSchema = schema || getState().tables.currentSchema;
  body.args[0].args.where.function_schema = fnSchema;
  body.args[1].args.where.function_schema = fnSchema;
  body.args[2].args.where.function_schema = fnSchema;

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
        consistentFunctions = filterInconsistentMetadataObjects(
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

const updateCurrentSchema = (schemaName, redirect = true) => dispatch => {
  if (redirect) {
    dispatch(_push(getSchemaBaseRoute(schemaName)));
  }

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
  if (globals.consoleMode === SERVER_CONSOLE_MODE) {
    // handle errors for run_sql based workflow
    dispatch(showErrorNotification(title, errorMsg.code, errorMsg));
  } else if (errorMsg.code === 'migration_failed') {
    dispatch(showErrorNotification(title, 'Migration Failed', errorMsg));
  } else if (errorMsg.code === 'data_api_error') {
    const parsedErrorMsg = errorMsg;
    parsedErrorMsg.message = JSON.parse(errorMsg.message);
    dispatch(
      showErrorNotification(title, parsedErrorMsg.message.error, parsedErrorMsg)
    );
  } else {
    // any other unhandled codes
    const parsedErrorMsg = errorMsg;
    parsedErrorMsg.message = JSON.parse(errorMsg.message);
    dispatch(showErrorNotification(title, errorMsg.code, parsedErrorMsg));
  }
};

const makeMigrationCall = (
  dispatch,
  getState,
  upQueries,
  downQueries = [],
  migrationName,
  customOnSuccess,
  customOnError,
  requestMsg,
  successMsg,
  errorMsg,
  shouldSkipSchemaReload,
  skipExecution = false,
  isRetry
) => {
  const upQuery = {
    type: 'bulk',
    args: upQueries,
  };

  const downQuery = {
    type: 'bulk',
    args:
      downQueries.length > 0 ? downQueries : getDownQueryComments(upQueries),
  };

  const migrationBody = {
    name: sanitize(migrationName),
    up: upQuery.args,
    down: downQuery.args,
    skip_execution: skipExecution,
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
    if (!shouldSkipSchemaReload) {
      if (globals.consoleMode === CLI_CONSOLE_MODE) {
        dispatch(loadMigrationStatus()); // don't call for server mode
      }
      dispatch(updateSchemaInfo());
    }
    if (successMsg) {
      dispatch(showSuccessNotification(successMsg));
    }
    customOnSuccess(data, globals.consoleMode, currMigrationMode);
  };
  const retryMigration = (err = {}, errMsg = '', isPgCascade = false) => {
    const errorDetails = getErrorMessage('', err);
    const errorDetailsLines = errorDetails.split('\n');

    dispatch(
      showNotification(
        {
          title: errMsg,
          level: 'error',
          message: (
            <p>
              {errorDetailsLines.map((m, i) => (
                <div key={i}>{m}</div>
              ))}
              <br />
              Do you want to drop the dependent items as well?
            </p>
          ),
          autoDismiss: 0,
          action: {
            label: 'Continue',
            callback: () =>
              makeMigrationCall(
                dispatch,
                getState,
                cascadeUpQueries(upQueries, isPgCascade), // cascaded new up queries
                downQueries,
                migrationName,
                customOnSuccess,
                customOnError,
                requestMsg,
                successMsg,
                errorMsg,
                shouldSkipSchemaReload,
                false,
                true // prevent further retry
              ),
          },
        },
        'error'
      )
    );
  };

  const onError = err => {
    if (!isRetry) {
      const { dependencyError, pgDependencyError } = getDependencyError(err);
      if (dependencyError) return retryMigration(dependencyError, errorMsg);
      if (pgDependencyError)
        return retryMigration(pgDependencyError, errorMsg, true);
    }

    dispatch(handleMigrationErrors(errorMsg, err));
    customOnError(err);
  };

  dispatch({ type: MAKE_REQUEST });
  dispatch(showSuccessNotification(requestMsg));
  dispatch(requestAction(url, options, REQUEST_SUCCESS, REQUEST_ERROR)).then(
    onSuccess,
    onError
  );
};

const getBulkColumnInfoFetchQuery = schema => {
  const fetchColumnTypes = getRunSqlQuery(fetchColumnTypesQuery, false, true);
  const fetchTypeDefaultValues = getRunSqlQuery(
    fetchColumnDefaultFunctions(schema),
    false,
    true
  );
  const fetchValidTypeCasts = getRunSqlQuery(
    fetchColumnCastsQuery,
    false,
    true
  );

  return {
    type: 'bulk',
    args: [fetchColumnTypes, fetchTypeDefaultValues, fetchValidTypeCasts],
  };
};

const fetchColumnTypeInfo = () => {
  return (dispatch, getState) => {
    const url = Endpoints.getSchema;
    const currState = getState();
    const { currentSchema } = currState.tables;
    const reqQuery = getBulkColumnInfoFetchQuery(currentSchema);
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(reqQuery),
    };
    return dispatch(requestAction(url, options)).then(
      data => {
        const resultData = data[1].result.slice(1);
        const typeFuncsMap = {};

        resultData.forEach(r => {
          typeFuncsMap[r[1]] = r[0].split(',');
        });
        const columnDataTypeInfo = {
          columnDataTypes: data[0].result.slice(1),
          columnTypeDefaultValues: typeFuncsMap,
          columnTypeCasts: convertArrayToJson(data[2].result.slice(1)),
        };
        return dispatch({
          type: FETCH_COLUMN_TYPE_INFO,
          data: columnDataTypeInfo,
        });
      },
      error => {
        dispatch(
          showErrorNotification('Error fetching column types', null, error)
        );
        return dispatch({
          type: FETCH_COLUMN_TYPE_INFO_FAIL,
          data: error,
        });
      }
    );
  };
};

export const fetchRoleList = () => (dispatch, getState) => {
  const query = getFetchAllRolesQuery();
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(query),
  };

  return dispatch(requestAction(Endpoints.query, options)).then(
    data => {
      const allRoles = [...new Set(data.map(r => r.role_name))];
      const { inconsistentObjects } = getState().metadata;

      let consistentRoles = [...allRoles];

      if (inconsistentObjects.length > 0) {
        consistentRoles = filterInconsistentMetadataObjects(
          allRoles,
          inconsistentObjects,
          'roles'
        );
      }

      dispatch(setAllRoles(consistentRoles));
    },
    error => {
      console.error('Failed to load roles ' + JSON.stringify(error));
    }
  );
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
    case FETCH_COLUMN_TYPE_INFO:
      return {
        ...state,
        columnDataTypes: action.data.columnDataTypes,
        columnDefaultFunctions: action.data.columnTypeDefaultValues,
        columnDataTypeInfoErr: null,
        columnTypeCasts: action.data.columnTypeCasts,
      };

    case FETCH_COLUMN_TYPE_INFO_FAIL:
      return {
        ...state,
        columnDataTypes: [],
        columnDefaultFunctions: {},
        columnTypeCasts: {},
        columnDataTypeInfoErr: action.data,
      };
    case RESET_COLUMN_TYPE_INFO:
      return {
        ...state,
        columnDataTypes: [...defaultState.columnDataTypes],
        columnDefaultFunctions: { ...defaultState.columnDefaultFunctions },
        columnTypeCasts: { ...defaultState.columnTypeCasts },
        columnDataTypeInfoErr: defaultState.columnDataTypeInfoErr,
      };
    case SET_ALL_ROLES:
      return {
        ...state,
        allRoles: action.roles,
      };
    case SET_ADDITIONAL_COLUMNS_INFO:
      if (isEmpty(action.data)) return state;
      return {
        ...state,
        allSchemas: state.allSchemas.map(schema => {
          if (!action.data[schema.table_name]) return schema;
          return {
            ...schema,
            columns: schema.columns.map(column => ({
              ...column,
              ...action.data[schema.table_name][column.column_name],
            })),
          };
        }),
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
  fetchColumnTypeInfo,
  RESET_COLUMN_TYPE_INFO,
  setUntrackedRelations,
  SET_ADDITIONAL_COLUMNS_INFO,
  fetchAdditionalColumnsInfo,
};
