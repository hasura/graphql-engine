import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import dataHeaders from '../Common/Headers';
import requestAction from '../../../../utils/requestAction';
import defaultState from './AddState';
import _push from '../push';
import {
  loadTriggers,
  makeMigrationCall,
  setTrigger,
  loadProcessedEvents,
} from '../EventActions';
import { showSuccessNotification } from '../Notification';
import { UPDATE_MIGRATION_STATUS_ERROR } from '../../../Main/Actions';

const SET_DEFAULTS = 'AddTrigger/SET_DEFAULTS';
const SET_TRIGGERNAME = 'AddTrigger/SET_TRIGGERNAME';
const SET_TABLENAME = 'AddTrigger/SET_TABLENAME';
const SET_SCHEMANAME = 'AddTrigger/SET_SCHEMANAME';
const SET_WEBHOOK_URL = 'AddTrigger/SET_WEBHOOK_URL';
const SET_RETRY_NUM = 'AddTrigger/SET_RETRY_NUM';
const SET_RETRY_INTERVAL = 'AddTrigger/SET_RETRY_INTERVAL';
const MAKING_REQUEST = 'AddTrigger/MAKING_REQUEST';
const REQUEST_SUCCESS = 'AddTrigger/REQUEST_SUCCESS';
const REQUEST_ERROR = 'AddTrigger/REQUEST_ERROR';
const VALIDATION_ERROR = 'AddTrigger/VALIDATION_ERROR';
const UPDATE_TABLE_LIST = 'AddTrigger/UPDATE_TABLE_LIST';
const TOGGLE_COLUMNS = 'AddTrigger/TOGGLE_COLUMNS';
const TOGGLE_QUERY_TYPE_SELECTED = 'AddTrigger/TOGGLE_QUERY_TYPE_SELECTED';
const TOGGLE_QUERY_TYPE_DESELECTED = 'AddTrigger/TOGGLE_QUERY_TYPE_DESELECTED';

const setTriggerName = value => ({ type: SET_TRIGGERNAME, value });
const setTableName = value => ({ type: SET_TABLENAME, value });
const setSchemaName = value => ({ type: SET_SCHEMANAME, value });
const setWebhookURL = value => ({ type: SET_WEBHOOK_URL, value });
const setRetryNum = value => ({ type: SET_RETRY_NUM, value });
const setRetryInterval = value => ({ type: SET_RETRY_INTERVAL, value });
const setDefaults = () => ({ type: SET_DEFAULTS });
// General error during validation.
// const validationError = (error) => ({type: VALIDATION_ERROR, error: error});
const validationError = error => {
  alert(error);
  return { type: VALIDATION_ERROR, error };
};

const createTrigger = () => {
  return (dispatch, getState) => {
    dispatch({ type: MAKING_REQUEST });
    dispatch(showSuccessNotification('Creating Trigger...'));
    const currentState = getState().addTrigger;
    const currentSchema = currentState.schemaName;
    const triggerName = currentState.triggerName;
    const tableName = currentState.tableName;
    const webhook = currentState.webhookURL;

    // apply migrations
    const migrationName = 'create_trigger_' + triggerName.trim();
    const payload = {
      type: 'create_event_trigger',
      args: {
        name: triggerName,
        table: { name: tableName, schema: currentSchema },
        webhook: webhook,
      },
    };
    const downPayload = {
      type: 'delete_event_trigger',
      args: {
        name: triggerName,
      },
    };
    // operation definition
    if (currentState.selectedOperations.insert) {
      payload.args.insert = { columns: currentState.operations.insert };
    }
    if (currentState.selectedOperations.update) {
      payload.args.update = { columns: currentState.operations.update };
    }
    if (currentState.selectedOperations.delete) {
      payload.args.delete = { columns: currentState.operations.delete };
    }
    // retry logic
    if (currentState.retryConf) {
      payload.args.retry_conf = currentState.retryConf;
    }
    const upQueryArgs = [];
    upQueryArgs.push(payload);
    const downQueryArgs = [];
    downQueryArgs.push(downPayload);
    const upQuery = {
      type: 'bulk',
      args: upQueryArgs,
    };
    const downQuery = {
      type: 'bulk',
      args: downQueryArgs,
    };
    const requestMsg = 'Creating trigger...';
    const successMsg = 'Trigger Created';
    const errorMsg = 'Create trigger failed';

    const customOnSuccess = () => {
      // dispatch({ type: REQUEST_SUCCESS });

      dispatch(setTrigger(triggerName.trim()));
      dispatch(loadTriggers()).then(() => {
        dispatch(loadProcessedEvents()).then(() => {
          dispatch(
            _push('/manage/triggers/' + triggerName.trim() + '/processed')
          );
        });
      });
      return;
    };
    const customOnError = err => {
      dispatch({ type: REQUEST_ERROR, data: errorMsg });
      dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
      return;
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

const fetchTableListBySchema = schemaName => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify({
      type: 'select',
      args: {
        table: {
          name: 'hdb_table',
          schema: 'hdb_catalog',
        },
        columns: ['*.*'],
        where: { table_schema: schemaName },
      },
    }),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: UPDATE_TABLE_LIST, data: data });
    },
    error => {
      console.error('Failed to load triggers' + JSON.stringify(error));
    }
  );
};

const operationToggleColumn = (column, operation) => {
  return (dispatch, getState) => {
    const currentOperations = getState().addTrigger.operations;
    const currentCols = currentOperations[operation];
    // check if column is in currentCols. if not, push
    const isExists = currentCols.includes(column);
    let finalCols = currentCols;
    if (isExists) {
      finalCols = currentCols.filter(col => col !== column);
    } else {
      finalCols.push(column);
    }
    dispatch({ type: TOGGLE_COLUMNS, cols: finalCols, op: operation });
  };
};

const operationToggleAllColumns = columns => {
  return dispatch => {
    dispatch({ type: TOGGLE_COLUMNS, cols: columns, op: 'insert' });
    dispatch({ type: TOGGLE_COLUMNS, cols: columns, op: 'update' });
    dispatch({ type: TOGGLE_COLUMNS, cols: columns, op: 'delete' });
  };
};

const setOperationSelection = (type, isChecked) => {
  return dispatch => {
    if (isChecked) {
      dispatch({ type: TOGGLE_QUERY_TYPE_SELECTED, data: type });
    } else {
      dispatch({ type: TOGGLE_QUERY_TYPE_DESELECTED, data: type });
    }
  };
};

const addTriggerReducer = (state = defaultState, action) => {
  switch (action.type) {
    case SET_DEFAULTS:
      return {
        ...defaultState,
        operations: {
          ...defaultState.operations,
          insert: [],
          update: [],
          delete: [],
        },
        selectedOperations: {
          ...defaultState.selectedOperations,
          insert: false,
          update: false,
          delete: false,
        },
      };
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
    case VALIDATION_ERROR:
      return { ...state, internalError: action.error, lastSuccess: null };
    case SET_TRIGGERNAME:
      return { ...state, triggerName: action.value };
    case SET_WEBHOOK_URL:
      return { ...state, webhookURL: action.value };
    case SET_RETRY_NUM:
      return {
        ...state,
        retryConf: {
          ...state.retryConf,
          num_retries: parseInt(action.value, 10),
        },
      };
    case SET_RETRY_INTERVAL:
      return {
        ...state,
        retryConf: {
          ...state.retryConf,
          interval_sec: parseInt(action.value, 10),
        },
      };
    case SET_TABLENAME:
      return { ...state, tableName: action.value };
    case SET_SCHEMANAME:
      return { ...state, schemaName: action.value };
    case UPDATE_TABLE_LIST:
      return { ...state, tableListBySchema: action.data };
    case TOGGLE_COLUMNS:
      const operations = state.operations;
      operations[action.op] = action.cols;
      return { ...state, operations: { ...operations } };
    case TOGGLE_QUERY_TYPE_SELECTED:
      const selectedOperations = state.selectedOperations;
      selectedOperations[action.data] = true;
      return { ...state, selectedOperations: { ...selectedOperations } };
    case TOGGLE_QUERY_TYPE_DESELECTED:
      const deselectedOperations = state.selectedOperations;
      deselectedOperations[action.data] = false;
      return { ...state, selectedOperations: { ...deselectedOperations } };
    default:
      return state;
  }
};

export default addTriggerReducer;
export {
  setTriggerName,
  setTableName,
  setSchemaName,
  setWebhookURL,
  setRetryNum,
  setRetryInterval,
  createTrigger,
  fetchTableListBySchema,
  operationToggleColumn,
  operationToggleAllColumns,
  setOperationSelection,
  setDefaults,
};
export { validationError };
