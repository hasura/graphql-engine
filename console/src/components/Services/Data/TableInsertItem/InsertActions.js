import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import requestAction from '../../../../utils/requestAction';
import { Reals } from '../constants';

import {
  showErrorNotification,
  showNotification,
} from '../../Common/Notification';
import dataHeaders from '../Common/Headers';
import {
  getEnumColumnMappings,
  dataSource,
  findTable,
} from '../../../../dataSources';
import { getEnumOptionsQuery } from '../../../Common/utils/v1QueryUtils';
import { isStringArray } from '../../../Common/utils/jsUtils';
import {
  getInsertUpQuery,
  getInsertDownQuery,
} from '../../../Common/utils/v1QueryUtils';
import { makeMigrationCall } from '../DataActions';
import { removeAll } from 'react-notification-system-redux';
import { getNotificationDetails } from '../../Common/Notification';
import { getTableConfiguration } from '../TableBrowseRows/utils';

const I_SET_CLONE = 'InsertItem/I_SET_CLONE';
const I_RESET = 'InsertItem/I_RESET';
const I_ONGOING_REQ = 'InsertItem/I_ONGOING_REQ';
const I_REQUEST_SUCCESS = 'InsertItem/I_REQUEST_SUCCESS';
const I_REQUEST_ERROR = 'InsertItem/I_REQUEST_ERROR';
const _CLOSE = 'InsertItem/_CLOSE';
const _OPEN = 'InsertItem/_OPEN';
const I_FETCH_ENUM_OPTIONS_SUCCESS = 'InsertItem/I_FETCH_ENUM_SUCCESS';
const I_FETCH_ENUM_OPTIONS_ERROR = 'InsertItem/I_FETCH_ENUM_ERROR';

const Open = () => ({ type: _OPEN });
const Close = () => ({ type: _CLOSE });

const insertItemAsMigration = (
  tableInfo,
  insertedData,
  primaryKeyInfo,
  columns,
  callback
) => (dispatch, getState) => {
  const source = getState().tables.currentDataSource;

  const upQuery = getInsertUpQuery(
    { name: tableInfo.name, schema: tableInfo.schema },
    insertedData,
    columns,
    source
  );
  const downQuery = getInsertDownQuery(
    { name: tableInfo.name, schema: tableInfo.schema },
    insertedData,
    primaryKeyInfo,
    columns,
    source
  );

  const migrationName = `insert_into_${tableInfo.schema}_${tableInfo.name}`;
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

/* ****************** insert action creators ************ */
const insertItem = (tableName, colValues, isMigration = false) => {
  return (dispatch, getState) => {
    /* Type all the values correctly */
    dispatch({ type: I_ONGOING_REQ });
    const insertObject = {};
    const { tables, metadata } = getState();
    const { currentSchema, currentDataSource, allSchemas } = tables;
    const tableDef = { name: tableName, schema: currentSchema };
    const sources = metadata.metadataObject?.sources;
    const tableConfiguration = getTableConfiguration(tables, sources);
    const currentTableInfo = findTable(allSchemas, tableDef);
    if (!currentTableInfo) return;
    const columns = currentTableInfo.columns;
    let error = false;
    let errorMessage = '';
    Object.keys(colValues).map(colName => {
      const colSchema = columns.find(x => x.column_name === colName);
      const colType = colSchema.data_type;
      const colValue = colValues[colName];

      if (Reals.indexOf(colType) > 0) {
        insertObject[colName] = parseFloat(colValue, 10) || colValue;
      } else if (colType === dataSource.columnDataTypes.BOOLEAN) {
        if (colValue === 'true') {
          insertObject[colName] = true;
        } else if (colValue === 'false') {
          insertObject[colName] = false;
        } else {
          insertObject[colName] = null;
        }
      } else if (
        colType === dataSource.columnDataTypes.JSONDTYPE ||
        colType === dataSource.columnDataTypes.JSONB
      ) {
        try {
          const val = JSON.parse(colValue);
          insertObject[colName] = val;
        } catch (e) {
          errorMessage = `${colName} :: could not read ${colValue} as a valid JSON object/array`;
          error = true;
        }
      } else if (
        colType === dataSource.columnDataTypes.ARRAY &&
        isStringArray(colValue)
      ) {
        try {
          const arr = JSON.parse(colValue);
          insertObject[colName] = dataSource.arrayToPostgresArray(arr);
        } catch {
          errorMessage = `${colName} :: could not read ${colValue} as a valid JSON object/array`;
        }
      } else {
        insertObject[colName] = colValue;
      }
    });

    if (error) {
      dispatch(showErrorNotification('Insert failed!', errorMessage));
      return dispatch({
        type: I_REQUEST_ERROR,
        error: { message: 'Not valid JSON' },
      });
    }
    const returning = columns.map(col => col.column_name);
    if (!dataSource.generateInsertRequest) return;
    const {
      getInsertRequestBody,
      processInsertData,
      endpoint: url,
    } = dataSource.generateInsertRequest();

    const reqBody = getInsertRequestBody({
      source: currentDataSource,
      tableDef,
      tableConfiguration,
      insertObject,
      returning,
    });

    const options = {
      method: 'POST',
      credentials: globalCookiePolicy,
      headers: dataHeaders(getState),
      body: JSON.stringify(reqBody),
    };

    const migrationSuccessCB = (affectedRows, returnedFields) => {
      const detailsAction = {
        label: 'Details',
        callback: () => {
          dispatch(
            showNotification(
              {
                position: 'br',
                title: 'Inserted data!',
                message: `Affected rows: ${affectedRows}`,
                dismissible: 'button',
                autoDismiss: 0,
                children: getNotificationDetails(returnedFields),
              },
              'success'
            )
          );
        },
      };

      dispatch(
        showNotification(
          {
            title: 'Inserted data!',
            message: `Affected rows: ${affectedRows}`,
            action: detailsAction,
          },
          'success'
        )
      );
    };
    return dispatch(
      requestAction(url, options, I_REQUEST_SUCCESS, I_REQUEST_ERROR)
    ).then(
      data => {
        const { affectedRows, returnedFields } = processInsertData(
          data,
          tableConfiguration,
          {
            currentSchema,
            currentTable: tableName,
          }
        );
        if (isMigration) {
          dispatch(
            insertItemAsMigration(
              tableDef,
              returnedFields,
              currentTableInfo.primary_key,
              columns,
              () => migrationSuccessCB(affectedRows, returnedFields)
            )
          );
        } else {
          migrationSuccessCB(affectedRows, returnedFields);
        }
      },

      err => {
        dispatch(removeAll());
        dispatch(showErrorNotification('Insert failed!', err.error, err));
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
            type: I_FETCH_ENUM_OPTIONS_SUCCESS,
            data: {
              columnName: request.columnName,
              options: data.reduce(
                (acc, d) => [...acc, ...Object.values(d)],
                []
              ),
            },
          }),
        () => dispatch({ type: I_FETCH_ENUM_OPTIONS_ERROR })
      );
    });
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
        enumOptions: null,
      };
    case I_SET_CLONE:
      return {
        clone: action.clone,
        ongoingRequest: false,
        lastError: null,
        lastSuccess: null,
        enumOptions: null,
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
    case I_FETCH_ENUM_OPTIONS_SUCCESS:
      return {
        ...state,
        enumOptions: {
          ...state.enumOptions,
          [action.data.columnName]: action.data.options,
        },
      };
    case I_FETCH_ENUM_OPTIONS_ERROR:
      return { ...state, enumOptions: null };
    default:
      return state;
  }
};

export default insertReducer;
export { fetchEnumOptions, insertItem, I_SET_CLONE, I_RESET, Open, Close };
