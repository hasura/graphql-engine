import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import requestAction from 'utils/requestAction';
import { Integers, Reals } from '../constants';
import {
  showErrorNotification,
  showSuccessNotification,
} from '../../Common/Notification';
import dataHeaders from '../Common/Headers';

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
    /* Type all the values correctly */
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
      if (Integers.indexOf(colSchema.data_type) > 0) {
        insertObject[colName] = parseInt(colValues[colName], 10);
      } else if (Reals.indexOf(colSchema.data_type) > 0) {
        insertObject[colName] = parseFloat(colValues[colName], 10);
      } else if (colSchema.data_type === 'boolean') {
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
      dispatch(showErrorNotification('Edit failed!', errorMessage));
      return dispatch({
        type: E_REQUEST_ERROR,
        error: { message: 'Not valid JSON' },
      });
    }

    const reqBody = {
      type: 'update',
      args: {
        table: { name: tableName, schema: getState().tables.currentSchema },
        $set: insertObject,
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
