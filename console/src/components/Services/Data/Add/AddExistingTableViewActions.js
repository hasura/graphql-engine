import defaultState from './AddExistingTableViewState';
import _push from '../push';
import {
  updateSchemaInfo,
  fetchTrackedFunctions,
  makeMigrationCall,
} from '../DataActions';
import { showSuccessNotification } from '../../Common/Notification';
import {
  getSchemaBaseRoute,
  getTableBrowseRoute,
  getTableModifyRoute,
  getFunctionModifyRoute,
} from '../../../Common/utils/routesUtils';
import { checkIfTable } from '../../../Common/utils/pgUtils';
import Migration from '../../../../utils/migration/Migration';

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
    const tableName = state.tableName.trim();

    const requestBodyUp = {
      type: 'add_existing_table_or_view',
      args: {
        name: tableName,
        schema: currentSchema,
      },
    };
    const requestBodyDown = {
      type: 'untrack_table',
      args: {
        table: {
          name: tableName,
          schema: currentSchema,
        },
      },
    };

    const migration = new Migration();
    migration.add(requestBodyUp, requestBodyDown);
    const migrationName =
      'add_existing_table_or_view_' + currentSchema + '_' + tableName;

    const requestMsg = 'Adding existing table/view...';
    const successMsg = 'Existing table/view added';
    const errorMsg = 'Adding existing table/view failed';
    const customOnSuccess = () => {
      dispatch({ type: REQUEST_SUCCESS });
      dispatch(updateSchemaInfo()).then(() => {
        const newTable = getState().tables.allSchemas.find(
          t => t.table_name === tableName && t.table_schema === currentSchema
        );
        const isTable = checkIfTable(newTable);
        const nextRoute = isTable
          ? getTableModifyRoute(currentSchema, tableName, isTable)
          : getTableBrowseRoute(currentSchema, tableName, isTable);
        dispatch(_push(nextRoute));
      });
      return;
    };
    const customOnError = err => {
      dispatch({ type: REQUEST_ERROR, data: err });
    };

    makeMigrationCall({
      dispatch,
      getState,
      migration,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg,
    });
  };
};

const addExistingFunction = name => {
  return (dispatch, getState) => {
    dispatch({ type: MAKING_REQUEST });
    dispatch(showSuccessNotification('Adding an function...'));
    const currentSchema = getState().tables.currentSchema;

    const upMigration = {
      type: 'track_function',
      args: {
        name,
        schema: currentSchema,
      },
    };
    const downMigration = {
      type: 'untrack_function',
      args: {
        name,
        schema: currentSchema,
      },
    };
    const migration = new Migration();
    migration.add(upMigration, downMigration);

    const migrationName = 'add_existing_function ' + currentSchema + '_' + name;

    const requestMsg = 'Adding existing function...';
    const successMsg = 'Existing function added';
    const errorMsg = 'Adding existing function failed';
    const customOnSuccess = () => {
      dispatch({ type: REQUEST_SUCCESS });
      // Update the left side bar
      dispatch(fetchTrackedFunctions(currentSchema));
      dispatch(_push(getFunctionModifyRoute(currentSchema, name)));
      return;
    };
    const customOnError = err => {
      dispatch({ type: REQUEST_ERROR, data: err });
    };

    makeMigrationCall({
      dispatch,
      getState,
      migration,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg,
    });
  };
};

const addAllUntrackedTablesSql = tableList => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;

    dispatch({ type: MAKING_REQUEST });
    dispatch(showSuccessNotification('Existing table/view added!'));

    const migration = new Migration();
    for (let i = 0; i < tableList.length; i++) {
      if (tableList[i].table_name !== 'schema_migrations') {
        migration.add(
          {
            type: 'add_existing_table_or_view',
            args: {
              name: tableList[i].table_name,
              schema: currentSchema,
            },
          },
          {
            type: 'untrack_table',
            args: {
              table: {
                name: tableList[i].table_name,
                schema: currentSchema,
              },
            },
          }
        );
      }
    }
    const migrationName = 'add_all_existing_table_or_view_' + currentSchema;

    const requestMsg = 'Adding existing table/view...';
    const successMsg = 'Existing table/view added';
    const errorMsg = 'Adding existing table/view failed';
    const customOnSuccess = () => {
      dispatch(showSuccessNotification('Existing table/view added!'));
      dispatch({ type: REQUEST_SUCCESS });
      dispatch(updateSchemaInfo()).then(() => {
        dispatch(_push(getSchemaBaseRoute(currentSchema)));
      });
      return;
    };
    const customOnError = err => {
      dispatch({ type: REQUEST_ERROR, data: err });
    };

    makeMigrationCall({
      dispatch,
      getState,
      migration,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg,
      shouldSkipSchemaReload: true,
    });
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
