import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import requestAction from 'utils/requestAction';
import { Integers, Reals } from '../constants';
import {
  showErrorNotification,
  showSuccessNotification,
} from '../../Common/Notification';
import dataHeaders from '../Common/Headers';
import {
  findTable,
  generateTableDef,
  getColumnType,
  getTableColumn,
  getEnumColumnMappings,
  arrayToPostgresArray,
} from '../../../Common/utils/pgUtils';
import {
  getEnumOptionsQuery,
  getEditRowQuery,
} from '../../../Common/utils/v1QueryUtils';
import { ARRAY } from '../utils';
import { isStringArray } from '../../../Common/utils/jsUtils';
import { makeMigrationCall } from '../DataActions';

const E_SET_EDITITEM = 'EditItem/E_SET_EDITITEM';
const E_ONGOING_REQ = 'EditItem/E_ONGOING_REQ';
const E_REQUEST_SUCCESS = 'EditItem/E_REQUEST_SUCCESS';
const E_REQUEST_ERROR = 'EditItem/E_REQUEST_ERROR';
const E_FETCH_ENUM_OPTIONS_SUCCESS = 'EditItem/E_FETCH_ENUM_SUCCESS';
const E_FETCH_ENUM_OPTIONS_ERROR = 'EditItem/E_FETCH_ENUM_ERROR';
const MODAL_CLOSE = 'EditItem/MODAL_CLOSE';
const MODAL_OPEN = 'EditItem/MODAL_OPEN';

const modalOpen = () => ({ type: MODAL_OPEN });
const modalClose = () => ({ type: MODAL_CLOSE });

const editRowAsMigration = (
  tableDef,
  pkClause,
  oldItem,
  newItem,
  callback,
  columns
) => (dispatch, getState) => {
  const upQuery = getEditRowQuery(tableDef, newItem, columns, pkClause);
  const downQuery = getEditRowQuery(tableDef, oldItem, columns, pkClause);

  const migrationName = `update_row_in_${tableDef.schema}_${tableDef.name}`;
  const customOnSuccess = () => {
    if (callback) {
      callback();
    }
  };
  const customOnError = () => {};
  const requestMessage = 'Creating migration...';
  const successMessage = 'Migration created';
  const errorMessage = 'Creating migration failed';

  makeMigrationCall(
    dispatch,
    getState,
    [upQuery],
    [downQuery],
    migrationName,
    customOnSuccess,
    customOnError,
    requestMessage,
    successMessage,
    errorMessage,
    true,
    true
  );
};

/* ****************** edit action creators ************ */
const editItem = (tableName, colValues, isMigration = false) => {
  return (dispatch, getState) => {
    const state = getState();

    /* Type all the values correctly */
    const { currentSchema, allSchemas } = state.tables;

    const tableDef = generateTableDef(tableName, currentSchema);

    const table = findTable(allSchemas, tableDef);

    const currentTable = state.tables.allSchemas.find(tab => {
      return tab.table_name === tableName && tab.table_schema === currentSchema;
    });
   
    const _setObject = {};
    const _defaultArray = [];

    let errorMessage = '';

    if (!Object.keys(colValues).length) {
      errorMessage = 'No fields modified';
    }

    Object.keys(colValues).map(colName => {
      const colValue = colValues[colName];

      const column = getTableColumn(table, colName);
      const colType = getColumnType(column);

      if (colValue && colValue.default === true) {
        _defaultArray.push(colName);
      } else {
        if (Integers.indexOf(colType) > 0) {
          _setObject[colName] = parseInt(colValue, 10);
        } else if (Reals.indexOf(colType) > 0) {
          _setObject[colName] = parseFloat(colValue);
        } else if (colType === 'boolean') {
          if (colValue === 'true') {
            _setObject[colName] = true;
          } else if (colValue === 'false') {
            _setObject[colName] = false;
          } else {
            _setObject[colName] = null;
          }
        } else if (colType === 'json' || colType === 'jsonb') {
          try {
            _setObject[colName] = JSON.parse(colValue);
          } catch (e) {
            errorMessage = `${colName} :: could not read ${colValue} as a valid object/array`;
          }
        } else if (colType === ARRAY && isStringArray(colValue)) {
          try {
            const arr = JSON.parse(colValue);
            _setObject[colName] = arrayToPostgresArray(arr);
          } catch {
            errorMessage = `${colName} :: could not read ${colValue} as a valid array`;
          }
        } else {
          _setObject[colName] = colValue;
        }
      }
    });

    if (errorMessage) {
      dispatch(showErrorNotification('Edit failed!', errorMessage));
      return dispatch({
        type: E_REQUEST_ERROR,
        error: { message: errorMessage },
      });
    }
    let returning = [];
    if (isMigration) {
      returning = columns.map(col => col.column_name);
    }
    const { pkClause, oldItem } = state.tables.update;
    const reqBody = {
      type: 'update',
      args: {
        table: tableDef,
        $set: _setObject,
        $default: _defaultArray,
        where: pkClause,
        returning,
      },
    };
    const options = {
      method: 'POST',
      credentials: globalCookiePolicy,
      headers: dataHeaders(getState),
      body: JSON.stringify(reqBody),
    };
    const url = Endpoints.query;
    // call back
    const cb = affectedRows => {
      dispatch(
        showSuccessNotification(
          'Edited!',
          `Affected Rows: ${affectedRows}`,
          true
        )
      );
    };

    return dispatch(
      requestAction(url, options, E_REQUEST_SUCCESS, E_REQUEST_ERROR)
    ).then(
      data => {
        const affectedRows = data.affected_rows;
        if (!isMigration) {
          cb(affectedRows);
        } else {
          dispatch(
            editRowAsMigration(
              tableDef,
              pkClause,
              oldItem,
              data.returning[0],
              () => cb(affectedRows),
              columns
            )
          );
        }
      },
      err => {
        dispatch(showErrorNotification('Edit failed!', err.error, err));
      }
    );
  };
};

const fetchEnumOptions = () => {
  return (dispatch, getState) => {
    const {
      tables: { allSchemas, currentTable, currentSchema },
    } = getState();

    const requests = getEnumColumnMappings(
      allSchemas,
      currentTable,
      currentSchema
    );

    if (!requests) return;

    const options = {
      method: 'POST',
      credentials: globalCookiePolicy,
      headers: dataHeaders(getState),
    };
    const url = Endpoints.query;

    requests.forEach(request => {
      const req = getEnumOptionsQuery(request, currentSchema);

      return dispatch(
        requestAction(url, {
          ...options,
          body: JSON.stringify(req),
        })
      ).then(
        data =>
          dispatch({
            type: E_FETCH_ENUM_OPTIONS_SUCCESS,
            data: {
              columnName: request.columnName,
              options: data.reduce(
                (acc, d) => [...acc, ...Object.values(d)],
                []
              ),
            },
          }),
        () => dispatch({ type: E_FETCH_ENUM_OPTIONS_ERROR })
      );
    });
  };
};

/* ************ reducers *********************** */
const editReducer = (tableName, state, action) => {
  switch (action.type) {
    case E_SET_EDITITEM:
      return {
        ongoingRequest: false,
        lastError: null,
        lastSuccess: null,
        oldItem: action.oldItem,
        pkClause: action.pkClause,
      };
    case E_ONGOING_REQ:
      return {
        ...state,
        ongoingRequest: true,
        lastError: null,
        lastSuccess: null,
      };
    case E_REQUEST_SUCCESS:
      return {
        ...state,
        ongoingRequest: false,
        lastError: null,
        lastSuccess: action.data,
      };
    case E_REQUEST_ERROR:
      if (action.data) {
        return {
          ...state,
          ongoingRequest: false,
          lastError: action.data,
          lastSuccess: null,
        };
      }
      return {
        ...state,
        ongoingRequest: false,
        lastError: 'server-failure',
        lastSuccess: null,
      };
    case E_FETCH_ENUM_OPTIONS_SUCCESS:
      return {
        ...state,
        enumOptions: {
          ...state.enumOptions,
          [action.data.columnName]: action.data.options,
        },
      };
    case E_FETCH_ENUM_OPTIONS_ERROR:
      return { ...state, enumOptions: null };
    case MODAL_OPEN:
      return { ...state, isModalOpen: true };
    case MODAL_CLOSE:
      return { ...state, isModalOpen: false };
    default:
      return state;
  }
};

export default editReducer;
export {
  editItem,
  fetchEnumOptions,
  modalOpen,
  modalClose,
  E_SET_EDITITEM,
  E_ONGOING_REQ,
};
