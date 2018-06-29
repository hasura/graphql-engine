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

const SET_TABLE = 'Data/SET_TABLE';
const LOAD_SCHEMA = 'Data/LOAD_SCHEMA';
const LOAD_UNTRACKED_SCHEMA = 'Data/LOAD_UNTRACKED_SCHEMA';
const LOAD_TABLE_COMMENT = 'Data/LOAD_TABLE_COMMENT';
const LISTING_SCHEMA = 'Data/LISTING_SCHEMA';
const LOAD_UNTRACKED_RELATIONS = 'Data/LOAD_UNTRACKED_RELATIONS';
const FETCH_SCHEMA_LIST = 'Data/FETCH_SCHEMA_LIST';
const UPDATE_CURRENT_SCHEMA = 'Data/UPDATE_CURRENT_SCHEMA';

const MAKE_REQUEST = 'ModifyTable/MAKE_REQUEST';
const REQUEST_SUCCESS = 'ModifyTable/REQUEST_SUCCESS';
const REQUEST_ERROR = 'ModifyTable/REQUEST_ERROR';

/* ************ action creators *********************** */
const fetchSchemaList = () => dispatch => {
  const url = Endpoints.getSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders,
    body: JSON.stringify({
      type: 'select',
      args: {
        table: {
          name: 'schemata',
          schema: 'information_schema',
        },
        columns: ['schema_name'],
      },
    }),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: FETCH_SCHEMA_LIST, schemaList: data });
    },
    error => {
      console.error('Failed to fetch schema');
      console.error(error);
      dispatch(
        showErrorNotification('Schema Error', 'Could not fetch schema.')
      );
    }
  );
};

const loadSchema = () => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const currentSchema = getState().tables.currentSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders,
    body: JSON.stringify({
      type: 'select',
      args: {
        table: {
          name: 'hdb_table',
          schema: 'hdb_catalog',
        },
        columns: ['*.*'],
        where: { table_schema: currentSchema },
      },
    }),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: LOAD_SCHEMA, allSchemas: data });
    },
    error => {
      console.error('Failed to load schema');
      console.error(error);
      dispatch(
        showErrorNotification(
          'Schema Error',
          'Could not load schema. Please refresh the page!'
        )
      );
    }
  );
};

const loadUntrackedSchema = () => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const currentSchema = getState().tables.currentSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders,
    body: JSON.stringify({
      type: 'select',
      args: {
        table: {
          name: 'tables',
          schema: 'information_schema',
        },
        columns: ['table_name'],
        where: {
          table_schema: currentSchema,
        },
      },
    }),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: LOAD_UNTRACKED_SCHEMA, untrackedSchemas: data });
    },
    error => {
      console.error('Failed to load schema');
      console.error(error);
      dispatch(
        showErrorNotification(
          'Schema Error',
          'Could not load public schema. Please refresh the page!'
        )
      );
    }
  );
};

const loadUntrackedRelations = () => (dispatch, getState) => {
  dispatch(loadSchema()).then(() => {
    const untrackedRelations = getAllUnTrackedRelations(
      getState().tables.allSchemas,
      getState().tables.currentSchema
    );
    dispatch({
      type: LOAD_UNTRACKED_RELATIONS,
      untrackedRelations,
    });
  });
};

const fetchTableComment = tableName => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const currentSchema = getState().tables.currentSchema;
  const commentSql = `select description from pg_description join pg_class on pg_description.objoid = pg_class.oid join pg_namespace on pg_class.relnamespace = pg_namespace.oid where relname = '${tableName}' and nspname='${currentSchema}' `;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders,
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

const setTable = tableName => ({ type: SET_TABLE, tableName });

/* **********Shared functions between table actions********* */

const handleMigrationErrors = (title, errorMsg) => dispatch => {
  const requestMsg = title;
  if (globals.consoleMode === 'hasuradb') {
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
  errorMsg
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
  if (globals.consoleMode === 'hasuradb') {
    finalReqBody = upQuery;
  } else if (globals.consoleMode === 'cli') {
    finalReqBody = migrationBody;
  }
  const url = migrateUrl;
  const options = {
    method: 'POST',
    credentials: globalCookiePolicy,
    headers: dataHeaders,
    body: JSON.stringify(finalReqBody),
  };

  const onSuccess = () => {
    dispatch(loadMigrationStatus());
    dispatch(loadSchema());
    customOnSuccess();
    if (successMsg) {
      dispatch(showSuccessNotification(successMsg));
    }
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
    case LISTING_SCHEMA:
      return { ...state, listingSchemas: action.updatedSchemas };
    case SET_TABLE:
      return { ...state, currentTable: action.tableName };
    case FETCH_SCHEMA_LIST:
      return { ...state, schemaList: action.schemaList };
    case UPDATE_CURRENT_SCHEMA:
      return { ...state, currentSchema: action.currentSchema };
    default:
      return state;
  }
};

export default dataReducer;
export {
  setTable,
  loadSchema,
  loadUntrackedSchema,
  fetchTableComment,
  handleMigrationErrors,
  makeMigrationCall,
  LISTING_SCHEMA,
  LOAD_UNTRACKED_RELATIONS,
  UPDATE_CURRENT_SCHEMA,
  loadUntrackedRelations,
  fetchSchemaList,
};
