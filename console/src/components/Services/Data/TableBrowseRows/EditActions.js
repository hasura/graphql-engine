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
} from '../../../Common/utils/pgUtils';

const E_SET_EDITITEM = 'EditItem/E_SET_EDITITEM';
const E_ONGOING_REQ = 'EditItem/E_ONGOING_REQ';
const E_REQUEST_SUCCESS = 'EditItem/E_REQUEST_SUCCESS';
const E_REQUEST_ERROR = 'EditItem/E_REQUEST_ERROR';
const MODAL_CLOSE = 'EditItem/MODAL_CLOSE';
const MODAL_OPEN = 'EditItem/MODAL_OPEN';

const modalOpen = () => ({ type: MODAL_OPEN });
const modalClose = () => ({ type: MODAL_CLOSE });

/* ****************** edit action creators ************ */
const editItem = (tableName, colValues) => {
  return (dispatch, getState) => {
    const state = getState();

    /* Type all the values correctly */
    const { currentSchema, allSchemas } = state.tables;

    const tableDef = generateTableDef(tableName, currentSchema);

    const table = findTable(allSchemas, tableDef);

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
            errorMessage =
              colName +
              ' :: could not read ' +
              colValue +
              ' as a valid JSON object/array';
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

    const reqBody = {
      type: 'update',
      args: {
        table: tableDef,
        $set: _setObject,
        $default: _defaultArray,
        where: state.tables.update.pkClause,
      },
    };
    const options = {
      method: 'POST',
      credentials: globalCookiePolicy,
      headers: dataHeaders(getState),
      body: JSON.stringify(reqBody),
    };
    const url = Endpoints.query;

    return dispatch(
      requestAction(url, options, E_REQUEST_SUCCESS, E_REQUEST_ERROR)
    ).then(
      data => {
        dispatch(
          showSuccessNotification(
            'Edited!',
            'Affected rows: ' + data.affected_rows
          )
        );
      },
      err => {
        dispatch(showErrorNotification('Edit failed!', err.error, err));
      }
    );
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
    case MODAL_OPEN:
      return { ...state, isModalOpen: true };
    case MODAL_CLOSE:
      return { ...state, isModalOpen: false };
    default:
      return state;
  }
};

export default editReducer;
export { editItem, modalOpen, modalClose, E_SET_EDITITEM, E_ONGOING_REQ };
