import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import requestAction from '../../../../utils/requestAction';
import { Integers, Reals } from '../constants';
import {
  showErrorNotification,
  showSuccessNotification,
} from '../../Common/Notification';
import dataHeaders from '../Common/Headers';
import {
  findTable,
  getTableColumn,
  getEnumColumnMappings,
  dataSource,
} from '../../../../dataSources';
import { getEnumOptionsQuery } from '../../../Common/utils/v1QueryUtils';
import { isStringArray } from '../../../Common/utils/jsUtils';
import { generateTableDef } from '../../../../dataSources';
import { getTableConfiguration } from './utils';

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

/* ****************** edit action creators ************ */
const editItem = (tableName, colValues) => {
  return (dispatch, getState) => {
    const { tables, metadata } = getState();
    const sources = metadata.metadataObject?.sources;
    const tableConfiguration = getTableConfiguration(tables, sources);
    /* Type all the values correctly */
    const { currentSchema, allSchemas, currentDataSource } = tables;

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
      const colType = dataSource.getColumnType(column);

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
        } else if (
          colType === dataSource.columnDataTypes.JSONB ||
          colType === dataSource.columnDataTypes.JSONDTYPE
        ) {
          try {
            _setObject[colName] = JSON.parse(colValue);
          } catch (e) {
            errorMessage =
              colName +
              ' :: could not read ' +
              colValue +
              ' as a valid JSON object/array';
          }
        } else if (
          colType === dataSource.columnDataTypes.ARRAY &&
          isStringArray(colValue)
        ) {
          try {
            const arr = JSON.parse(colValue);
            _setObject[colName] = dataSource.arrayToPostgresArray(arr);
          } catch {
            errorMessage =
              colName + ' :: could not read ' + colValue + ' as a valid array';
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

    if (!dataSource.generateEditRowRequest) return;

    const {
      getEditRowRequestBody,
      processEditData,
      endpoint: url,
    } = dataSource.generateEditRowRequest();

    const reqBody = getEditRowRequestBody({
      source: currentDataSource,
      tableDef,
      tableConfiguration,
      set: _setObject,
      defaultArray: _defaultArray,
      where: tables.update.pkClause,
    });

    const options = {
      method: 'POST',
      credentials: globalCookiePolicy,
      headers: dataHeaders(getState),
      body: JSON.stringify(reqBody),
    };

    return dispatch(
      requestAction(url, options, E_REQUEST_SUCCESS, E_REQUEST_ERROR)
    ).then(
      data => {
        dispatch(
          showSuccessNotification(
            'Edited!',
            'Affected rows: ' +
              processEditData({ data, tableDef, tableConfiguration })
          )
        );
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
      tables: { allSchemas, currentTable, currentSchema, currentDataSource },
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
      const req = getEnumOptionsQuery(
        request,
        currentSchema,
        currentDataSource
      );

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
