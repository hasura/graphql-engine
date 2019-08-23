import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import requestAction from 'utils/requestAction';
import { Integers, Reals } from '../constants';

import {
  showErrorNotification,
  showSuccessNotification,
} from '../../Common/Notification';
import dataHeaders from '../Common/Headers';

const I_SET_CLONE = 'InsertItem/I_SET_CLONE';
const I_RESET = 'InsertItem/I_RESET';
const I_ONGOING_REQ = 'InsertItem/I_ONGOING_REQ';
const I_REQUEST_SUCCESS = 'InsertItem/I_REQUEST_SUCCESS';
const I_REQUEST_ERROR = 'InsertItem/I_REQUEST_ERROR';
const _CLOSE = 'InsertItem/_CLOSE';
const _OPEN = 'InsertItem/_OPEN';

const Open = () => ({ type: _OPEN });
const Close = () => ({ type: _CLOSE });

/* ****************** insert action creators ************ */
const insertItem = (tableName, colValues) => {
  return (dispatch, getState) => {
    /* Type all the values correctly */
    dispatch({ type: I_ONGOING_REQ });
    const insertObject = {};
    const state = getState();
    const { currentSchema } = state.tables;
    const columns = state.tables.allSchemas.find(
      t => t.table_name === tableName && t.table_schema === currentSchema
    ).columns;
    let error = false;
    let errorMessage = '';
    Object.keys(colValues).map(colName => {
      const colSchema = columns.find(x => x.column_name === colName);
      const colType = colSchema.data_type;
      if (Integers.indexOf(colType) > 0) {
        insertObject[colName] =
          parseInt(colValues[colName], 10) || colValues[colName];
      } else if (Reals.indexOf(colType) > 0) {
        insertObject[colName] =
          parseFloat(colValues[colName], 10) || colValues[colName];
      } else if (colType === 'boolean') {
        if (colValues[colName] === 'true') {
          insertObject[colName] = true;
        } else if (colValues[colName] === 'false') {
          insertObject[colName] = false;
        } else {
          insertObject[colName] = null;
        }
      } else if (colType === 'json' || colType === 'jsonb') {
        try {
          const val = JSON.parse(colValues[colName]);
          insertObject[colName] = val;
        } catch (e) {
          errorMessage =
            colName +
            ' :: could not read ' +
            colValues[colName] +
            ' as a valid JSON object/array';
          error = true;
        }
      } else {
        insertObject[colName] = colValues[colName];
      }
    });

    if (error) {
      dispatch(showErrorNotification('Insert failed!', errorMessage));
      return dispatch({
        type: I_REQUEST_ERROR,
        error: { message: 'Not valid JSON' },
      });
    }
    const reqBody = {
      type: 'insert',
      args: {
        table: { name: tableName, schema: getState().tables.currentSchema },
        objects: [insertObject],
        returning: [],
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
      requestAction(url, options, I_REQUEST_SUCCESS, I_REQUEST_ERROR)
    ).then(
      data => {
        dispatch(
          showSuccessNotification(
            'Inserted!',
            'Affected rows: ' + data.affected_rows
          )
        );
      },
      err => {
        dispatch(showErrorNotification('Insert failed!', err.error, err));
      }
    );
  };
};

/* ************ reducers *********************** */
const insertReducer = (tableName, state, action) => {
  switch (action.type) {
    case I_RESET:
      return {
        clone: null,
        ongoingRequest: false,
        lastError: null,
        lastSuccess: null,
      };
    case I_SET_CLONE:
      return {
        clone: action.clone,
        ongoingRequest: false,
        lastError: null,
        lastSuccess: null,
      };
    case I_ONGOING_REQ:
      return {
        ...state,
        ongoingRequest: true,
        lastError: null,
        lastSuccess: null,
      };
    case I_REQUEST_SUCCESS:
      return {
        ...state,
        clone: null,
        ongoingRequest: false,
        lastError: null,
        lastSuccess: action.data,
      };
    case I_REQUEST_ERROR:
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
    case _OPEN:
      return { ...state, isOpen: true };
    case _CLOSE:
      return { ...state, isOpen: false };
    default:
      return state;
  }
};

export default insertReducer;
export { insertItem, I_SET_CLONE, I_RESET, Open, Close };
