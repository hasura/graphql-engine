import defaultState from './AddExistingTableViewState';
import _push from '../push';
import { updateSchemaInfo, makeMigrationCall } from '../DataActions';
import { showSuccessNotification } from '../../Common/Notification';
import {
  getSchemaBaseRoute,
  getTableBrowseRoute,
  getTableModifyRoute,
  getFunctionModifyRoute,
} from '../../../Common/utils/routesUtils';
import { dataSource, currentDriver } from '../../../../dataSources';
import {
  findTable,
  escapeTableColumns,
  escapeTableName,
  getQualifiedTableDef,
} from '../../../../dataSources/common';
import { exportMetadata } from '../../../../metadata/actions';
import {
  getUntrackTableQuery,
  getTrackFunctionQuery,
  getUntrackFunctionQuery,
  getTrackTableQuery,
} from '../../../../metadata/queryUtils';
import { setSidebarLoading } from '../DataSubSidebar';

const SET_DEFAULTS = 'AddExistingTable/SET_DEFAULTS';
const SET_TABLENAME = 'AddExistingTable/SET_TABLENAME';
const MAKING_REQUEST = 'AddExistingTable/MAKING_REQUEST';
const REQUEST_SUCCESS = 'AddExistingTable/REQUEST_SUCCESS';
const REQUEST_ERROR = 'AddExistingTable/REQUEST_ERROR';

const setDefaults = () => ({ type: SET_DEFAULTS });
const setTableName = value => ({ type: SET_TABLENAME, value });

const addExistingTableSql = (name, customSchema, skipRouting = false) => {
  return (dispatch, getState) => {
    dispatch(setSidebarLoading(true));
    dispatch({ type: MAKING_REQUEST });
    dispatch(showSuccessNotification('Adding an existing table...'));
    const state = getState().addTable.existingTableView;
    const currentSchema = customSchema
      ? customSchema
      : getState().tables.currentSchema;
    const currentDataSource = getState().tables.currentDataSource;
    const tableName = name ? name : state.tableName.trim();

    const tableDef = getQualifiedTableDef(
      {
        name: tableName,
        schema: currentSchema,
      },
      currentDriver
    );
    const { allSchemas } = getState().tables;
    const table = findTable(allSchemas, tableDef);
    const requestBodyUp = getTrackTableQuery({
      tableDef,
      source: currentDataSource,
      customColumnNames: escapeTableColumns(table),
      customName: escapeTableName(tableName),
    });

    const requestBodyDown = getUntrackTableQuery(tableDef, currentDataSource);

    const migrationName = `add_existing_table_or_view_${currentSchema}_${tableName}`;

    const requestMsg = 'Adding existing table/view...';
    const successMsg = 'Existing table/view added';
    const errorMsg = 'Adding existing table/view failed';
    const customOnSuccess = () => {
      dispatch({ type: REQUEST_SUCCESS });
      dispatch(updateSchemaInfo()).then(() => {
        const newTable = getState().tables.allSchemas.find(
          t => t.table_name === tableName && t.table_schema === currentSchema
        );
        const isTableType = dataSource.isTable(newTable);
        const nextRoute =
          isTableType && currentDriver !== 'bigquery'
            ? getTableModifyRoute(
                currentSchema,
                currentDataSource,
                tableName,
                isTableType
              )
            : getTableBrowseRoute(
                currentSchema,
                currentDataSource,
                tableName,
                isTableType
              );
        if (!skipRouting) {
          dispatch(_push(nextRoute));
        }
        dispatch(setSidebarLoading(false));
      });
      return;
    };
    const customOnError = err => {
      dispatch({ type: REQUEST_ERROR, data: err });
      dispatch(setSidebarLoading(false));
    };

    return makeMigrationCall(
      dispatch,
      getState,
      [requestBodyUp],
      [requestBodyDown],
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const addExistingFunction = (
  name,
  config,
  customSchema,
  skipRouting = false
) => {
  return (dispatch, getState) => {
    dispatch(setSidebarLoading(true));
    dispatch({ type: MAKING_REQUEST });
    const currentSchema = customSchema
      ? customSchema
      : getState().tables.currentSchema;
    const currentDataSource = getState().tables.currentDataSource;

    const requestBodyUp = getTrackFunctionQuery(
      name,
      currentSchema,
      currentDataSource,
      config
    );
    const requestBodyDown = getUntrackFunctionQuery(
      name,
      currentSchema,
      currentDataSource
    );

    const migrationName = `add_existing_function_${currentSchema}_${name}`;

    const requestMsg = 'Adding existing function...';
    const successMsg = 'Existing function added';
    const errorMsg = 'Adding existing function failed';
    const customOnSuccess = () => {
      dispatch({ type: REQUEST_SUCCESS });
      // Update the left side bar
      dispatch(exportMetadata());
      if (!skipRouting) {
        dispatch(
          _push(getFunctionModifyRoute(currentSchema, currentDataSource, name))
        );
      }
      dispatch(setSidebarLoading(false));
    };
    const customOnError = err => {
      dispatch({ type: REQUEST_ERROR, data: err });
      dispatch(setSidebarLoading(false));
    };

    return makeMigrationCall(
      dispatch,
      getState,
      [requestBodyUp],
      [requestBodyDown],
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
    const currentDataSource = getState().tables.currentDataSource;

    dispatch({ type: MAKING_REQUEST });
    dispatch(showSuccessNotification('Adding...'));
    const bulkQueryUp = [];
    const bulkQueryDown = [];

    for (let i = 0; i < tableList.length; i++) {
      if (tableList[i].table_name !== 'schema_migrations') {
        const tableDef = getQualifiedTableDef(
          {
            name: tableList[i].table_name,
            schema: currentSchema,
          },
          currentDriver
        );

        const table = findTable(getState().tables.allSchemas, tableDef);
        bulkQueryUp.push(
          getTrackTableQuery({
            tableDef,
            source: currentDataSource,
            customColumnNames: escapeTableColumns(table),
            customName: escapeTableName(tableList[i].table_name),
          })
        );
        bulkQueryDown.push(
          getUntrackTableQuery(
            {
              table: {
                name: tableList[i].table_name,
                [currentDriver === 'bigquery'
                  ? 'dataset'
                  : 'schema']: currentSchema,
              },
            },
            currentDataSource
          )
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
        dispatch(_push(getSchemaBaseRoute(currentSchema, currentDataSource)));
      });
      return;
    };
    const customOnError = err => {
      dispatch({ type: REQUEST_ERROR, data: err });
    };

    makeMigrationCall(
      dispatch,
      getState,
      bulkQueryUp,
      bulkQueryDown,
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
