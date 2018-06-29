import defaultState from './AddExistingTableViewState';
import _push from '../push';
import {
  loadSchema,
  LOAD_UNTRACKED_RELATIONS,
  makeMigrationCall,
} from '../DataActions';
import { showSuccessNotification } from '../Notification';
import { loadMigrationStatus } from '../../../Main/Actions.js';
import { getAllUnTrackedRelations } from '../TableRelationships/Actions';

import globals from '../../../../Globals';

const SET_DEFAULTS = 'AddExistingTable/SET_DEFAULTS';
const SET_TABLENAME = 'AddExistingTable/SET_TABLENAME';
const MAKING_REQUEST = 'AddExistingTable/MAKING_REQUEST';
const REQUEST_SUCCESS = 'AddExistingTable/REQUEST_SUCCESS';
const REQUEST_ERROR = 'AddExistingTable/REQUEST_ERROR';

const setDefaults = () => ({ type: SET_DEFAULTS });
const setTableName = value => ({ type: SET_TABLENAME, value });

const addExistingTableSql = () => {
  return (dispatch, getState) => {
    dispatch({ type: MAKING_REQUEST });
    dispatch(showSuccessNotification('Adding an existing table...'));
    const state = getState().addTable.existingTableView;
    const currentSchema = getState().tables.currentSchema;

    const requestBody = {
      type: 'add_existing_table_or_view',
      args: {
        name: state.tableName.trim(),
        schema: currentSchema,
      },
    };
    const migrationName =
      'add_existing_table_or_view_' +
      currentSchema +
      '_' +
      state.tableName.trim();
    const upQuery = {
      type: 'bulk',
      args: [requestBody],
    };

    const schemaMigration = {
      name: migrationName,
      up: upQuery.args,
      down: [],
    };
    let finalReqBody = schemaMigration.up;
    if (globals.consoleMode === 'hasuradb') {
      finalReqBody = schemaMigration.up;
    }
    const requestMsg = 'Adding existing table/view...';
    const successMsg = 'Existing table/view added';
    const errorMsg = 'Adding existing table/view failed';
    const customOnSuccess = () => {
      dispatch(loadMigrationStatus());
      dispatch(showSuccessNotification('Existing table/view added!'));
      dispatch({ type: REQUEST_SUCCESS });
      dispatch(loadSchema()).then(() => {
        const newTable = getState().tables.allSchemas.find(
          t => t.table_name === state.tableName.trim()
        );
        const isTable = newTable.detail.table_type === 'BASE TABLE';
        if (isTable) {
          dispatch(
            _push(
              '/schema/' +
                currentSchema +
                '/tables/' +
                state.tableName.trim() +
                '/modify'
            )
          );
        } else {
          dispatch(
            _push(
              '/schema/' +
                currentSchema +
                '/views/' +
                state.tableName.trim() +
                '/browse'
            )
          );
        }
      });
      return;
    };
    const customOnError = err => {
      dispatch({ type: REQUEST_ERROR, data: err });
    };

    makeMigrationCall(
      dispatch,
      getState,
      finalReqBody,
      [],
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const addAllUntrackedTablesSql = tableList => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;

    dispatch({ type: MAKING_REQUEST });
    dispatch(showSuccessNotification('Existing table/view added!'));
    const bulkQuery = [];
    for (let i = 0; i < tableList.length; i++) {
      if (tableList[i].table_name !== 'schema_migrations') {
        bulkQuery.push({
          type: 'add_existing_table_or_view',
          args: {
            name: tableList[i].table_name,
            schema: currentSchema,
          },
        });
      }
    }
    const migrationName = 'add_all_existing_table_or_view_' + currentSchema;
    const upQuery = {
      type: 'bulk',
      args: bulkQuery,
    };

    const schemaMigration = {
      name: migrationName,
      up: upQuery.args,
      down: [],
    };
    let finalReqBody = schemaMigration.up;
    if (globals.consoleMode === 'hasuradb') {
      finalReqBody = schemaMigration.up;
    }
    const requestMsg = 'Adding existing table/view...';
    const successMsg = 'Existing table/view added';
    const errorMsg = 'Adding existing table/view failed';
    const customOnSuccess = () => {
      dispatch(loadMigrationStatus());
      dispatch(showSuccessNotification('Existing table/view added!'));
      dispatch({ type: REQUEST_SUCCESS });
      dispatch(loadSchema()).then(() => {
        const allSchemas = getState().tables.allSchemas;
        const untrackedRelations = getAllUnTrackedRelations(allSchemas);
        dispatch({
          type: LOAD_UNTRACKED_RELATIONS,
          untrackedRelations: untrackedRelations,
        });
        dispatch(_push('/schema/' + currentSchema));
      });
      return;
    };
    const customOnError = err => {
      dispatch({ type: REQUEST_ERROR, data: err });
    };

    makeMigrationCall(
      dispatch,
      getState,
      finalReqBody,
      [],
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const addExistingTableReducer = (state = defaultState, action) => {
  switch (action.type) {
    case SET_DEFAULTS:
      return { ...defaultState };
    case MAKING_REQUEST:
      return {
        ...state,
        ongoingRequest: true,
        lastError: null,
        lastSuccess: null,
      };
    case REQUEST_SUCCESS:
      return {
        ...state,
        ongoingRequest: false,
        lastError: null,
        lastSuccess: true,
      };
    case REQUEST_ERROR:
      return {
        ...state,
        ongoingRequest: false,
        lastError: action.data,
        lastSuccess: null,
      };
    case SET_TABLENAME:
      return { ...state, tableName: action.value };
    default:
      return state;
  }
};

export default addExistingTableReducer;
export {
  setDefaults,
  setTableName,
  addExistingTableSql,
  addAllUntrackedTablesSql,
};
