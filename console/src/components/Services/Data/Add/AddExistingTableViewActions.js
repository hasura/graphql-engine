import defaultState from './AddExistingTableViewState';
import _push from '../push';
import {
  updateSchemaInfo,
  fetchTrackedFunctions,
  makeMigrationCall,
} from '../DataActions';
import { showSuccessNotification } from '../../Common/Notification';

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

    const requestBodyUp = {
      type: 'add_existing_table_or_view',
      args: {
        name: state.tableName.trim(),
        schema: currentSchema,
      },
    };
    const requestBodyDown = {
      type: 'untrack_table',
      args: {
        table: {
          name: state.tableName.trim(),
          schema: currentSchema,
        },
      },
    };
    const migrationName =
      'add_existing_table_or_view_' +
      currentSchema +
      '_' +
      state.tableName.trim();
    const upQuery = {
      type: 'bulk',
      args: [requestBodyUp],
    };
    const downQuery = {
      type: 'bulk',
      args: [requestBodyDown],
    };

    const requestMsg = 'Adding existing table/view...';
    const successMsg = 'Existing table/view added';
    const errorMsg = 'Adding existing table/view failed';
    const customOnSuccess = () => {
      dispatch({ type: REQUEST_SUCCESS });
      dispatch(updateSchemaInfo()).then(() => {
        const newTable = getState().tables.allSchemas.find(
          t =>
            t.table_name === state.tableName.trim() &&
            t.table_schema === currentSchema
        );
        const isTable = newTable.table_type === 'BASE TABLE';
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
      upQuery.args,
      downQuery.args,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const addExistingFunction = name => {
  return (dispatch, getState) => {
    dispatch({ type: MAKING_REQUEST });
    dispatch(showSuccessNotification('Adding an function...'));
    const currentSchema = getState().tables.currentSchema;

    const requestBodyUp = {
      type: 'track_function',
      args: {
        name,
        schema: currentSchema,
      },
    };
    const requestBodyDown = {
      type: 'untrack_function',
      args: {
        name,
        schema: currentSchema,
      },
    };
    const migrationName = 'add_existing_function ' + currentSchema + '_' + name;
    const upQuery = {
      type: 'bulk',
      args: [requestBodyUp],
    };
    const downQuery = {
      type: 'bulk',
      args: [requestBodyDown],
    };

    const requestMsg = 'Adding existing function...';
    const successMsg = 'Existing function added';
    const errorMsg = 'Adding existing function failed';
    const customOnSuccess = () => {
      dispatch({ type: REQUEST_SUCCESS });
      // Update the left side bar
      dispatch(fetchTrackedFunctions(currentSchema));
      dispatch(
        _push('/schema/' + currentSchema + '/functions/' + name + '/modify')
      );
      return;
    };
    const customOnError = err => {
      dispatch({ type: REQUEST_ERROR, data: err });
    };

    makeMigrationCall(
      dispatch,
      getState,
      upQuery.args,
      downQuery.args,
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
    const bulkQueryUp = [];
    const bulkQueryDown = [];
    for (let i = 0; i < tableList.length; i++) {
      if (tableList[i].table_name !== 'schema_migrations') {
        bulkQueryUp.push({
          type: 'add_existing_table_or_view',
          args: {
            name: tableList[i].table_name,
            schema: currentSchema,
          },
        });
        bulkQueryDown.push({
          type: 'untrack_table',
          args: {
            table: {
              name: tableList[i].table_name,
              schema: currentSchema,
            },
          },
        });
      }
    }
    const migrationName = 'add_all_existing_table_or_view_' + currentSchema;
    const upQuery = {
      type: 'bulk',
      args: bulkQueryUp,
    };
    const downQuery = {
      type: 'bulk',
      args: bulkQueryDown,
    };

    const requestMsg = 'Adding existing table/view...';
    const successMsg = 'Existing table/view added';
    const errorMsg = 'Adding existing table/view failed';
    const customOnSuccess = () => {
      dispatch(showSuccessNotification('Existing table/view added!'));
      dispatch({ type: REQUEST_SUCCESS });
      dispatch(updateSchemaInfo()).then(() => {
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
      upQuery.args,
      downQuery.args,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg,
      true
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
  addExistingFunction,
  setDefaults,
  setTableName,
  addExistingTableSql,
  addAllUntrackedTablesSql,
};
