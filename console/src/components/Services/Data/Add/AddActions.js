import defaultState from './AddState';

import _push from '../push';
import { updateSchemaInfo, makeMigrationCall } from '../DataActions';
import { showErrorNotification } from '../../Common/Notification';
import { UPDATE_MIGRATION_STATUS_ERROR } from '../../../Main/Actions';
import { setTable } from '../DataActions.js';
import { getTableModifyRoute } from '../../../Common/utils/routesUtils';
import { dataSource } from '../../../../dataSources';
import {
  findTable,
  escapeTableColumns,
  escapeTableName,
} from '../../../../dataSources/common';
import { getRunSqlQuery } from '../../../Common/utils/v1QueryUtils';
import { exportMetadata } from '../../../../metadata/actions';
import {
  getTrackTableQuery,
  getUntrackTableQuery,
} from '../../../../metadata/queryUtils';
import { setSidebarLoading } from '../DataSubSidebar';

const SET_DEFAULTS = 'AddTable/SET_DEFAULTS';
const SET_TABLENAME = 'AddTable/SET_TABLENAME';
const SET_TABLECOMMENT = 'AddTable/SET_TABLECOMMENT';
const REMOVE_COLUMN = 'AddTable/REMOVE_COLUMN';
const SET_COLUMNS_BULK = 'AddTable/SET_COLUMNS_BULK';
const SET_COLNAME = 'AddTable/SET_COLNAME';
const SET_COLTYPE = 'AddTable/SET_COLTYPE';
const SET_COLDEFAULT = 'AddTable/SET_COLDEFAULT';
const REMOVE_COLDEFAULT = 'AddTable/REMOVE_COLDEFAULT';
const SET_COLNULLABLE = 'AddTable/SET_COLNULLABLE';
const SET_COLUNIQUE = 'AddTable/SET_COLUNIQUE';
const ADD_COL = 'AddTable/ADD_COL';
const SET_PK = 'AddTable/SET_PK';
const SET_FKS = 'AddTable/SET_FKS';
const SET_UNIQUE_KEYS = 'AddTable/SET_UNIQUE_KEYS';
const TOGGLE_FK = 'AddTable/TOGGLE_FK';
const CLEAR_FK_TOGGLE = 'AddTable/CLEAR_FK_TOGGLE';
const MAKING_REQUEST = 'AddTable/MAKING_REQUEST';
const REQUEST_SUCCESS = 'AddTable/REQUEST_SUCCESS';
const REQUEST_ERROR = 'AddTable/REQUEST_ERROR';
const VALIDATION_ERROR = 'AddTable/VALIDATION_ERROR';
const RESET_VALIDATION_ERROR = 'AddTable/RESET_VALIDATION_ERROR';
const SET_CHECK_CONSTRAINTS = 'AddTable/SET_CHECK_CONSTRAINTS';
const REMOVE_CHECK_CONSTRAINT = 'AddTable/REMOVE_CHECK_CONSTRAINT';

const setCheckConstraints = constraints => ({
  type: SET_CHECK_CONSTRAINTS,
  constraints,
});
const removeCheckConstraint = index => ({
  type: REMOVE_CHECK_CONSTRAINT,
  index,
});
const setDefaults = () => ({ type: SET_DEFAULTS });
const setTableName = value => ({ type: SET_TABLENAME, value });
const setTableComment = value => ({ type: SET_TABLECOMMENT, value });
const removeColumn = i => ({ type: REMOVE_COLUMN, index: i });
const setColName = (name, index, isNull) => ({
  type: SET_COLNAME,
  name,
  index,
  isNull,
});
const setColDefault = (colDefault, index, isNull) => ({
  type: SET_COLDEFAULT,
  colDefault,
  index,
  isNull,
});
const setColType = (coltype, index) => ({
  type: SET_COLTYPE,
  coltype,
  index,
});
const removeColDefault = index => ({ type: REMOVE_COLDEFAULT, index });
const setColNullable = (isNull, index) => ({
  type: SET_COLNULLABLE,
  isNull,
  index,
});

const setFreqUsedColumn = column => (dispatch, getState) => {
  const tableState = getState().addTable.table;
  const columns = [...tableState.columns];

  const newColumn = {
    name: column.name,
    type: column.type,
    nullable: false,
  };

  if (column.default) {
    newColumn.default = { __type: 'value', value: column.default };
  }

  if (column.dependentSQLGenerator) {
    newColumn.dependentSQLGenerator = column.dependentSQLGenerator;
  }

  const numExistingCols = columns.length;

  let newColIndex;
  if (
    !columns[numExistingCols - 1].name &&
    !columns[numExistingCols - 1].type
  ) {
    columns[numExistingCols - 1] = newColumn;
    newColIndex = numExistingCols - 1;
  } else {
    columns.push(newColumn);
    newColIndex = numExistingCols;
  }

  dispatch({ type: SET_COLUMNS_BULK, columns });

  if (column.primary) {
    const newPks = [newColIndex.toString(), ''];

    dispatch({ type: SET_PK, pks: newPks });
  }
};

const setColUnique = (isUnique, index) => ({
  type: SET_COLUNIQUE,
  isUnique,
  index,
});

const setUniqueKeys = keys => ({
  type: SET_UNIQUE_KEYS,
  data: keys,
});

const addCol = () => ({ type: ADD_COL });
const setPk = pks => ({ type: SET_PK, pks });
const setForeignKeys = fks => ({
  type: SET_FKS,
  fks,
});
const toggleFk = i => ({ type: TOGGLE_FK, data: i });
const clearFkToggle = () => ({ type: CLEAR_FK_TOGGLE });

// General error during validation.
const validationError = error => {
  alert(error);
  return { type: VALIDATION_ERROR, error };
};
const resetValidation = () => ({ type: RESET_VALIDATION_ERROR });

/**
 *
 * @param {{name: string, schema: string}} payload
 */
export const trackTable = payload => (dispatch, getState) => {
  dispatch({ type: MAKING_REQUEST });
  const { currentDataSource, allSchemas } = getState().tables;
  const table = findTable(allSchemas, payload);
  const requestBodyUp = getTrackTableQuery({
    tableDef: payload,
    source: currentDataSource,
    customColumnNames: escapeTableColumns(table),
    customName: escapeTableName(payload.name),
  });
  const requestBodyDown = getUntrackTableQuery(payload, currentDataSource);

  const migrationName = 'track_table_' + payload.schema + '_' + payload.name;

  const successMsg = 'Table created successfully';
  const errorMsg = 'Tracking a created table failed';

  const customOnSuccess = () => {
    dispatch({ type: REQUEST_SUCCESS });
    dispatch({ type: SET_DEFAULTS });
    dispatch(setTable(payload.name));
    dispatch(updateSchemaInfo()).then(() => {
      dispatch(
        _push(
          getTableModifyRoute(
            payload.schema,
            currentDataSource,
            payload.name,
            true
          )
        )
      );
      dispatch(setSidebarLoading(false));
    });
    return;
  };

  const customOnError = err => {
    dispatch({ type: REQUEST_ERROR, data: err });
    dispatch(setSidebarLoading(false));
  };

  makeMigrationCall(
    dispatch,
    getState,
    [requestBodyUp],
    [requestBodyDown],
    migrationName,
    customOnSuccess,
    customOnError,
    null,
    successMsg,
    errorMsg
  );
};

const createTableSql = () => {
  return (dispatch, getState) => {
    dispatch({ type: MAKING_REQUEST });
    dispatch(setSidebarLoading(true));

    const state = getState().addTable.table;
    const currentSchema = getState().tables.currentSchema;

    const {
      foreignKeys,
      uniqueKeys,
      checkConstraints,
      tableName,
      columns,
      tableComment,
      primaryKeys,
    } = state;
    const { currentDataSource } = getState().tables;

    // validations
    if (tableName === '') {
      alert('Table name cannot be empty');
    }

    const createTableQueries = dataSource.getCreateTableQueries(
      currentSchema,
      tableName,
      columns,
      primaryKeys,
      foreignKeys,
      uniqueKeys,
      checkConstraints,
      tableComment
    );

    if (createTableQueries.error) {
      dispatch(setSidebarLoading(false));
      return dispatch(
        showErrorNotification('Create table failed', createTableQueries.error)
      );
    }

    // apply migrations
    const migrationName = 'create_table_' + currentSchema + '_' + tableName;

    // up migration
    const upChanges = createTableQueries.map(sql =>
      getRunSqlQuery(sql, currentDataSource)
    );

    // down migration
    const sqlDropTable = dataSource.getDropTableSql(currentSchema, tableName);
    const downChanges = [getRunSqlQuery(sqlDropTable, currentDataSource)];

    // make request
    const requestMsg = 'Creating table...';
    const errorMsg = 'Create table failed';

    const customOnSuccess = () => {
      dispatch(exportMetadata()).then(() => {
        dispatch(trackTable({ schema: currentSchema, name: tableName }));
      });
    };
    const customOnError = err => {
      dispatch({ type: REQUEST_ERROR, data: errorMsg });
      dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
      dispatch(setSidebarLoading(false));
      return;
    };

    makeMigrationCall(
      dispatch,
      getState,
      upChanges,
      downChanges,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      null,
      errorMsg,
      true
    );
  };
};

const addTableReducerCore = (state = defaultState, action) => {
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
    case RESET_VALIDATION_ERROR:
      return { ...state, internalError: null, lastSuccess: null };
    case VALIDATION_ERROR:
      return { ...state, internalError: action.error, lastSuccess: null };
    case SET_TABLENAME:
      return { ...state, tableName: action.value };
    case SET_TABLECOMMENT:
      return { ...state, tableComment: action.value };
    case REMOVE_COLUMN:
      // Removes the index of the removed column from the array of primaryKeys.
      const primaryKeys = state.primaryKeys
        .map(primaryKeyIndex => {
          const pkiValue = parseInt(primaryKeyIndex, 10);
          if (pkiValue < action.index) {
            return primaryKeyIndex;
          }
          if (pkiValue > action.index) {
            return (pkiValue - 1).toString();
          }
        })
        .filter(pki => pki !== undefined);

      const uniqueKeys = state.uniqueKeys
        .map(uk => {
          const newUniqueKey = uk
            .map(c => {
              if (c > action.index) return c - 1;
              if (c < action.index) return c;
            })
            .filter(c => c !== undefined);
          return [...newUniqueKey];
        })
        .filter(uk => uk.length !== 0);

      return {
        ...state,
        columns: [
          ...state.columns.slice(0, action.index),
          ...state.columns.slice(action.index + 1),
        ],
        primaryKeys: [...primaryKeys, ''],
        uniqueKeys: [...uniqueKeys, []],
      };
    case SET_COLNAME:
      const i = action.index;
      return {
        ...state,
        columns: [
          ...state.columns.slice(0, i),
          { ...state.columns[i], name: action.name, nullable: action.isNull },
          ...state.columns.slice(i + 1),
        ],
      };
    case SET_COLTYPE:
      const ij = action.index;
      return {
        ...state,
        columns: [
          ...state.columns.slice(0, ij),
          {
            ...state.columns[ij],
            type: action.coltype,
          },
          ...state.columns.slice(ij + 1),
        ],
      };
    case SET_COLDEFAULT:
      const ik = action.index;
      let defaultObj = {};
      defaultObj = { __type: 'value', value: action.colDefault };
      return {
        ...state,
        columns: [
          ...state.columns.slice(0, ik),
          {
            ...state.columns[ik],
            default: defaultObj,
            nullable: action.isNull,
          },
          ...state.columns.slice(ik + 1),
        ],
      };
    case REMOVE_COLDEFAULT:
      const ind = action.index;
      const dumyState = { ...state };
      delete dumyState.columns[ind].default;
      return dumyState;
    case SET_COLNULLABLE:
      const k = action.index;
      return {
        ...state,
        columns: [
          ...state.columns.slice(0, k),
          { ...state.columns[k], nullable: action.isNull },
          ...state.columns.slice(k + 1),
        ],
      };
    case SET_COLUNIQUE:
      const colInd = action.index;
      return {
        ...state,
        columns: [
          ...state.columns.slice(0, colInd),
          { ...state.columns[colInd], unique: action.isUnique },
          ...state.columns.slice(colInd + 1),
        ],
      };
    case ADD_COL:
      return { ...state, columns: [...state.columns, { name: '', type: '' }] };
    case SET_PK:
      return {
        ...state,
        primaryKeys: action.pks,
      };
    case SET_FKS:
      return {
        ...state,
        foreignKeys: action.fks,
      };
    case TOGGLE_FK:
      return {
        ...state,
        fkToggled: action.data,
      };
    case CLEAR_FK_TOGGLE:
      return {
        ...state,
        fkToggled: null,
      };
    case SET_UNIQUE_KEYS:
      return {
        ...state,
        uniqueKeys: action.data,
      };
    case SET_COLUMNS_BULK:
      return {
        ...state,
        columns: action.columns,
      };
    case SET_CHECK_CONSTRAINTS:
      return {
        ...state,
        checkConstraints: action.constraints,
      };
    case REMOVE_CHECK_CONSTRAINT:
      return {
        ...state,
        checkConstraints: state.checkConstraints.filter(
          (_, idx) => idx !== action.index
        ),
      };
    default:
      return state;
  }
};

const isValidColumn = col => {
  return (
    col.name !== null && col.name !== '' && col.type !== null && col.type !== ''
  );
};

const needsNewColumn = columns => {
  return columns.length === 0 || isValidColumn(columns[columns.length - 1]);
};

const addACol = columns => {
  return [...columns, { name: '', type: '' }];
};

const addTableReducer = (state = defaultState, action) => {
  let newState = addTableReducerCore(state, action);

  // and now we do everything to make the model make sense
  if (needsNewColumn(newState.columns)) {
    newState = {
      ...newState,
      columns: addACol(newState.columns),
    };
  }

  return newState;
};

export default addTableReducer;
export {
  setDefaults,
  setTableName,
  setTableComment,
  removeColumn,
  setColName,
  setColType,
  setColNullable,
  setColUnique,
  setColDefault,
  removeColDefault,
  addCol,
  setPk,
  setForeignKeys,
  setUniqueKeys,
  createTableSql,
  toggleFk,
  clearFkToggle,
  setFreqUsedColumn,
  setCheckConstraints,
  removeCheckConstraint,
};
export { resetValidation, validationError };
