import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import requestAction from '../../../utils/requestAction';
import defaultState from './DataState';
import insertReducer from './TableInsertItem/InsertActions';
import viewReducer from './TableBrowseRows/ViewActions';
import editReducer from './TableBrowseRows/EditActions';
import modifyReducer from './TableCommon/TableReducer';
import { getAllUnTrackedRelations } from './TableRelationships/Actions';
import { showErrorNotification, showSuccessNotification } from './Notification';
import dataHeaders from './Common/Headers';
import { loadMigrationStatus } from '../../Main/Actions';
import returnMigrateUrl from './Common/getMigrateUrl';
import globals from '../../../Globals';

import { SERVER_CONSOLE_MODE } from '../../../constants';

const SET_TABLE = 'Data/SET_TABLE';
const LOAD_FUNCTIONS = 'Data/LOAD_FUNCTIONS';
const LOAD_NON_TRACKABLE_FUNCTIONS = 'Data/LOAD_NON_TRACKABLE_FUNCTIONS';
const LOAD_TRACKED_FUNCTIONS = 'Data/LOAD_TRACKED_FUNCTIONS';
const UPDATE_TRACKED_FUNCTIONS = 'Data/UPDATE_TRACKED_FUNCTIONS';
const LOAD_SCHEMA = 'Data/LOAD_SCHEMA';
const LOAD_UNTRACKED_SCHEMA = 'Data/LOAD_UNTRACKED_SCHEMA';
const LOAD_TABLE_COMMENT = 'Data/LOAD_TABLE_COMMENT';
const LOAD_COLUMN_COMMENT = 'Data/LOAD_COLUMN_COMMENT';
const LISTING_SCHEMA = 'Data/LISTING_SCHEMA';
const LOAD_UNTRACKED_RELATIONS = 'Data/LOAD_UNTRACKED_RELATIONS';
const FETCH_SCHEMA_LIST = 'Data/FETCH_SCHEMA_LIST';
const UPDATE_CURRENT_SCHEMA = 'Data/UPDATE_CURRENT_SCHEMA';
const ADMIN_SECRET_ERROR = 'Data/ADMIN_SECRET_ERROR';
const UPDATE_DATA_HEADERS = 'Data/UPDATE_DATA_HEADERS';
const UPDATE_MANUAL_REL_TABLE_LIST = 'Data/UPDATE_MANUAL_REL_TABLE_LIST';
const RESET_MANUAL_REL_TABLE_LIST = 'Data/RESET_MANUAL_REL_TABLE_LIST';
const UPDATE_REMOTE_SCHEMA_MANUAL_REL = 'Data/UPDATE_SCHEMA_MANUAL_REL';

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
  loadSchema: {
    type: 'select',
    args: {
      table: {
        name: 'hdb_table',
        schema: 'hdb_catalog',
      },
      columns: [
        '*.*',
        {
          name: 'columns',
          columns: ['*.*'],
          order_by: [{ column: 'column_name', type: 'asc', nulls: 'last' }],
        },
      ],
      where: { table_schema: '' },
      order_by: [{ column: 'table_name', type: 'asc', nulls: 'last' }],
    },
  },
  loadUntrackedSchema: {
    type: 'select',
    args: {
      table: {
        name: 'tables',
        schema: 'information_schema',
      },
      columns: ['table_name'],
      where: {
        table_schema: '',
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
        dispatch({ type: LOAD_TRACKED_FUNCTIONS, data: data });
      },
      error => {
        console.error('Failed to load schema ' + JSON.stringify(error));
      }
    );
  };
};

const fetchDataInit = () => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const body = {
    type: 'bulk',
    args: [
      initQueries.schemaList,
      initQueries.loadSchema,
      initQueries.loadUntrackedSchema,
    ],
  };

  // set schema in queries
  const currentSchema = getState().tables.currentSchema;
  body.args[1].args.where.table_schema = currentSchema;
  body.args[2].args.where.table_schema = currentSchema;

  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(body),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: FETCH_SCHEMA_LIST, schemaList: data[0] });
      dispatch({ type: LOAD_SCHEMA, allSchemas: data[1] });
      dispatch({ type: LOAD_UNTRACKED_SCHEMA, untrackedSchemas: data[2] });
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
      dispatch({ type: LOAD_TRACKED_FUNCTIONS, data: data[2] });
    },
    error => {
      console.error('Failed to fetch schema ' + JSON.stringify(error));
    }
  );
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

const loadSchema = () => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const currentSchema = getState().tables.currentSchema;
  const body = initQueries.loadSchema;
  body.args.where.table_schema = currentSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(body),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: LOAD_SCHEMA, allSchemas: data });
    },
    error => {
      console.error('Failed to load schema ' + JSON.stringify(error));
    }
  );
};

const fetchViewInfoFromInformationSchema = (schemaName, viewName) => (
  dispatch,
  getState
) => {
  const url = Endpoints.getSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify({
      type: 'select',
      args: {
        table: {
          name: 'views',
          schema: 'information_schema',
        },
        columns: [
          'is_updatable',
          'is_insertable_into',
          'is_trigger_updatable',
          'is_trigger_deletable',
          'is_trigger_insertable_into',
        ],
        where: {
          table_name: viewName,
          table_schema: schemaName,
        },
      },
    }),
  };
  return dispatch(requestAction(url, options));
};

const loadUntrackedSchema = () => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const currentSchema = getState().tables.currentSchema;
  const body = initQueries.loadUntrackedSchema;
  body.args.where.table_schema = currentSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(body),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: LOAD_UNTRACKED_SCHEMA, untrackedSchemas: data });
    },
    error => {
      console.error('Failed to load schema ' + JSON.stringify(error));
    }
  );
};

const loadUntrackedRelations = () => (dispatch, getState) => {
  dispatch(loadSchema()).then(() => {
    const untrackedRelations = getAllUnTrackedRelations(
      getState().tables.allSchemas,
      getState().tables.currentSchema
    ).bulkRelTrack;
    dispatch({
      type: LOAD_UNTRACKED_RELATIONS,
      untrackedRelations,
    });
  });
};

const fetchTableComment = tableName => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const currentSchema = getState().tables.currentSchema;
  const commentSql = `select obj_description('${currentSchema}.${tableName}'::regclass) as description from pg_class
    WHERE relkind = 'r' AND relname = '${tableName}'`;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify({
      type: 'run_sql',
      args: {
        sql: commentSql,
      },
    }),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: LOAD_TABLE_COMMENT, data });
    },
    error => {
      console.error('Failed to load table comment');
      console.error(error);
    }
  );
};

const fetchColumnComment = (tableName, colName) => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const currentSchema = getState().tables.currentSchema;
  const commentSql = `SELECT pgd.description FROM pg_catalog.pg_statio_all_tables as st
    inner join pg_catalog.pg_description pgd on (pgd.objoid=st.relid)
    inner join information_schema.columns c on (pgd.objsubid=c.ordinal_position
    and  c.table_schema=st.schemaname and c.table_name=st.relname)
    WHERE column_name = '${colName}' AND table_name = '${tableName}' AND table_schema = '${currentSchema}';`;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify({
      type: 'run_sql',
      args: {
        sql: commentSql,
      },
    }),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: LOAD_COLUMN_COMMENT, data, column: colName });
    },
    error => {
      console.error('Failed to load column comment');
      console.error(error);
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

  const onSuccess = () => {
    if (!shouldSkipSchemaReload) {
      if (globals.consoleMode === 'cli') {
        dispatch(loadMigrationStatus()); // don't call for server mode
      }
      dispatch(loadSchema());
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

const fetchTableListBySchema = schemaName => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify({
      type: 'select',
      args: {
        table: {
          name: 'hdb_table',
          schema: 'hdb_catalog',
        },
        columns: [
          '*.*',
          {
            name: 'columns',
            columns: ['*.*'],
            order_by: [{ column: 'column_name', type: 'asc', nulls: 'last' }],
          },
        ],
        where: { table_schema: schemaName },
        order_by: [{ column: 'table_name', type: 'asc', nulls: 'last' }],
      },
    }),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: UPDATE_MANUAL_REL_TABLE_LIST, data: data });
    },
    error => {
      console.error('Failed to load table list' + JSON.stringify(error));
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
        listedFunctions: action.data,
      };

    case UPDATE_TRACKED_FUNCTIONS:
      return {
        ...state,
        listedFunctions: [...action.data],
      };
    case LOAD_SCHEMA:
      return {
        ...state,
        allSchemas: action.allSchemas,
        listingSchemas: action.allSchemas,
      };
    case LOAD_UNTRACKED_SCHEMA:
      return {
        ...state,
        untrackedSchemas: action.untrackedSchemas,
        information_schema: action.untrackedSchemas,
      };
    case LOAD_UNTRACKED_RELATIONS:
      return {
        ...state,
        untrackedRelations: action.untrackedRelations,
      };
    case LOAD_TABLE_COMMENT:
      return { ...state, tableComment: action.data };
    case LOAD_COLUMN_COMMENT:
      const loadedComment = action.data ? action.data.result[1] || '' : '';
      return {
        ...state,
        columnComments: {
          ...state.columnComments,
          [action.column]: loadedComment,
        },
      };
    case LISTING_SCHEMA:
      return { ...state, listingSchemas: action.updatedSchemas };
    case SET_TABLE:
      return { ...state, currentTable: action.tableName };
    case FETCH_SCHEMA_LIST:
      return { ...state, schemaList: action.schemaList };
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
          relAdd: {
            ...state.modify.relAdd,
            manualRelInfo: {
              ...state.modify.relAdd.manualRelInfo,
              remoteSchema: action.data,
            },
          },
        },
      };
    case UPDATE_MANUAL_REL_TABLE_LIST:
      return {
        ...state,
        modify: {
          ...state.modify,
          relAdd: {
            ...state.modify.relAdd,
            manualRelInfo: {
              ...state.modify.relAdd.manualRelInfo,
              tables: action.data,
            },
          },
        },
      };
    case RESET_MANUAL_REL_TABLE_LIST:
      return {
        ...state,
        modify: {
          ...state.modify,
          relAdd: {
            ...state.modify.relAdd,
            manualRelInfo: { ...defaultState.modify.relAdd.manualRelInfo },
          },
        },
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
  loadSchema,
  loadUntrackedSchema,
  fetchTableComment,
  fetchColumnComment,
  handleMigrationErrors,
  makeMigrationCall,
  LISTING_SCHEMA,
  LOAD_UNTRACKED_RELATIONS,
  UPDATE_CURRENT_SCHEMA,
  loadUntrackedRelations,
  fetchSchemaList,
  fetchDataInit,
  fetchFunctionInit,
  ADMIN_SECRET_ERROR,
  UPDATE_DATA_HEADERS,
  UPDATE_REMOTE_SCHEMA_MANUAL_REL,
  fetchTableListBySchema,
  RESET_MANUAL_REL_TABLE_LIST,
  fetchViewInfoFromInformationSchema,
  fetchTrackedFunctions,
  UPDATE_TRACKED_FUNCTIONS,
  initQueries,
};
