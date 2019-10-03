import defaultState from './AddState';
import _push from '../push';
import { loadTriggers, makeMigrationCall, setTrigger } from '../EventActions';
import { showSuccessNotification } from '../../Common/Notification';
import { UPDATE_MIGRATION_STATUS_ERROR } from '../../../Main/Actions';
import { updateSchemaInfo } from '../../Data/DataActions';

const SET_DEFAULTS = 'AddTrigger/SET_DEFAULTS';
const SET_TRIGGERNAME = 'AddTrigger/SET_TRIGGERNAME';
const SET_TABLENAME = 'AddTrigger/SET_TABLENAME';
const SET_SCHEMANAME = 'AddTrigger/SET_SCHEMANAME';
const SET_WEBHOOK_URL = 'AddTrigger/SET_WEBHOOK_URL';
const SET_RETRY_NUM = 'AddTrigger/SET_RETRY_NUM';
const SET_RETRY_INTERVAL = 'AddTrigger/SET_RETRY_INTERVAL';
const SET_RETRY_TIMEOUT = 'AddTrigger/SET_RETRY_TIMEOUT';
const MAKING_REQUEST = 'AddTrigger/MAKING_REQUEST';
const REQUEST_SUCCESS = 'AddTrigger/REQUEST_SUCCESS';
const REQUEST_ERROR = 'AddTrigger/REQUEST_ERROR';
const VALIDATION_ERROR = 'AddTrigger/VALIDATION_ERROR';
const TOGGLE_COLUMNS = 'AddTrigger/TOGGLE_COLUMNS';
const TOGGLE_ALL_COLUMNS = 'AddTrigger/TOGGLE_ALL_COLUMNS';
const TOGGLE_OPERATION = 'AddTrigger/TOGGLE_OPERATION';
const TOGGLE_ENABLE_MANUAL_CONFIG = 'AddTrigger/TOGGLE_ENABLE_MANUAL_CONFIG';
// const TOGGLE_QUERY_TYPE_SELECTED = 'AddTrigger/TOGGLE_QUERY_TYPE_SELECTED';
// const TOGGLE_QUERY_TYPE_DESELECTED = 'AddTrigger/TOGGLE_QUERY_TYPE_DESELECTED';
const REMOVE_HEADER = 'AddTrigger/REMOVE_HEADER';
const SET_HEADERKEY = 'AddTrigger/SET_HEADERKEY';
const SET_HEADERTYPE = 'AddTrigger/SET_HEADERTYPE';
const SET_HEADERVALUE = 'AddTrigger/SET_HEADERVALUE';
const ADD_HEADER = 'AddTrigger/ADD_HEADER';
const UPDATE_WEBHOOK_URL_TYPE = 'AddTrigger/UPDATE_WEBHOOK_URL_TYPE';

const setTriggerName = value => ({ type: SET_TRIGGERNAME, value });
const setTableName = value => ({ type: SET_TABLENAME, value });
const setSchemaName = value => ({ type: SET_SCHEMANAME, value });
const setWebhookURL = value => ({ type: SET_WEBHOOK_URL, value });
const setRetryNum = value => ({ type: SET_RETRY_NUM, value });
const setRetryInterval = value => ({ type: SET_RETRY_INTERVAL, value });
const setRetryTimeout = value => ({ type: SET_RETRY_TIMEOUT, value });
const setDefaults = () => ({ type: SET_DEFAULTS });
const addHeader = () => ({ type: ADD_HEADER });
const removeHeader = i => ({ type: REMOVE_HEADER, index: i });
const setHeaderKey = (key, index) => ({
  type: SET_HEADERKEY,
  key,
  index,
});
const setHeaderType = (headerType, index) => ({
  type: SET_HEADERTYPE,
  headerType,
  index,
});
const setHeaderValue = (headerValue, index) => ({
  type: SET_HEADERVALUE,
  headerValue,
  index,
});

// General error during validation.
// const validationError = (error) => ({type: VALIDATION_ERROR, error: error});
const validationError = error => {
  alert(error);
  return { type: VALIDATION_ERROR, error };
};

const getWebhookKey = (type, val) => {
  return { [type === 'url' ? 'webhook' : 'webhook_from_env']: val };
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
    const webhookType = currentState.webhookUrlType;

    // apply migrations
    const migrationName = 'create_trigger_' + triggerName.trim();
    const payload = {
      type: 'create_event_trigger',
      args: {
        name: triggerName,
        table: { name: tableName, schema: currentSchema },
        ...getWebhookKey(webhookType, webhook),
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
    if ('enableManual' in currentState) {
      payload.args.enable_manual = currentState.enableManual;
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

    payload.args.retry_conf = {
      num_retries:
        currentState.retryConf.num_retries === ''
          ? 0
          : parseInt(currentState.retryConf.num_retries, 10),
      interval_sec:
        currentState.retryConf.interval_sec === ''
          ? 10
          : parseInt(currentState.retryConf.interval_sec, 10),
      timeout_sec:
        currentState.retryConf.timeout_sec === ''
          ? 60
          : parseInt(currentState.retryConf.timeout_sec, 10),
    };

    // create header payload
    const headers = [];
    currentState.headers.map(header => {
      if (header.key !== '' && header.type !== '') {
        if (header.type === 'static') {
          headers.push({ name: header.key, value: header.value });
        } else if (header.type === 'env') {
          headers.push({ name: header.key, value_from_env: header.value });
        }
      }
    });
    payload.args.headers = headers;
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
      dispatch(setTrigger(triggerName.trim()));
      dispatch(loadTriggers([triggerName])).then(() => {
        dispatch(
          _push('/manage/triggers/' + triggerName.trim() + '/processed')
        );
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
      errorMsg,
      true
    );
  };
};

const loadTableList = schemaName => {
  return dispatch => dispatch(updateSchemaInfo({ schemas: [schemaName] }));
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
    dispatch({ type: TOGGLE_ALL_COLUMNS, cols: columns });
  };
};

const setOperationSelection = type => {
  return dispatch => {
    dispatch({ type: TOGGLE_OPERATION, data: type });
    /*
    if (isChecked) {
      dispatch({ type: TOGGLE_QUERY_TYPE_SELECTED, data: type });
    } else {
      dispatch({ type: TOGGLE_QUERY_TYPE_DESELECTED, data: type });
    }
    */
  };
};

const addTriggerReducer = (state = defaultState, action) => {
  switch (action.type) {
    case ADD_HEADER:
      return {
        ...state,
        headers: [...state.headers, { key: '', type: 'static', value: '' }],
      };
    case REMOVE_HEADER:
      return {
        ...state,
        headers: [
          ...state.headers.slice(0, action.index),
          ...state.headers.slice(action.index + 1),
        ],
      };
    case SET_HEADERKEY:
      const i = action.index;
      return {
        ...state,
        headers: [
          ...state.headers.slice(0, i),
          { ...state.headers[i], key: action.key },
          ...state.headers.slice(i + 1),
        ],
      };
    case SET_HEADERTYPE:
      const ij = action.index;
      return {
        ...state,
        headers: [
          ...state.headers.slice(0, ij),
          { ...state.headers[ij], type: action.headerType },
          ...state.headers.slice(ij + 1),
        ],
      };
    case SET_HEADERVALUE:
      const ik = action.index;
      return {
        ...state,
        headers: [
          ...state.headers.slice(0, ik),
          { ...state.headers[ik], value: action.headerValue },
          ...state.headers.slice(ik + 1),
        ],
      };
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
          num_retries: action.value,
        },
      };
    case SET_RETRY_INTERVAL:
      return {
        ...state,
        retryConf: {
          ...state.retryConf,
          interval_sec: action.value,
        },
      };
    case SET_RETRY_TIMEOUT:
      return {
        ...state,
        retryConf: {
          ...state.retryConf,
          timeout_sec: action.value,
        },
      };
    case SET_TABLENAME:
      return { ...state, tableName: action.value };
    case SET_SCHEMANAME:
      return { ...state, schemaName: action.value };
    case TOGGLE_COLUMNS:
      const operations = state.operations;
      operations[action.op] = action.cols;
      return { ...state, operations: { ...operations } };
    case TOGGLE_ALL_COLUMNS:
      return {
        ...state,
        operations: {
          insert: '*',
          delete: '*',
          update: action.cols,
        },
      };
    case TOGGLE_OPERATION:
      return {
        ...state,
        selectedOperations: {
          ...state.selectedOperations,
          [action.data]: !state.selectedOperations[action.data],
        },
      };

    case TOGGLE_ENABLE_MANUAL_CONFIG:
      return {
        ...state,
        enableManual: !state.enableManual,
      };
    /*
    case TOGGLE_QUERY_TYPE_SELECTED:
      const selectedOperations = state.selectedOperations;
      selectedOperations[action.data] = true;
      return { ...state, selectedOperations: { ...selectedOperations } };
    case TOGGLE_QUERY_TYPE_DESELECTED:
      const deselectedOperations = state.selectedOperations;
      deselectedOperations[action.data] = false;
      return { ...state, selectedOperations: { ...deselectedOperations } };
    */
    case UPDATE_WEBHOOK_URL_TYPE:
      return {
        ...state,
        webhookUrlType: action.data,
      };
    default:
      return state;
  }
};

export default addTriggerReducer;
export {
  addHeader,
  setHeaderKey,
  setHeaderValue,
  setHeaderType,
  removeHeader,
  setTriggerName,
  setTableName,
  setSchemaName,
  setWebhookURL,
  setRetryNum,
  setRetryInterval,
  setRetryTimeout,
  createTrigger,
  loadTableList,
  operationToggleColumn,
  operationToggleAllColumns,
  setOperationSelection,
  setDefaults,
  UPDATE_WEBHOOK_URL_TYPE,
  TOGGLE_ENABLE_MANUAL_CONFIG,
};
export { validationError };
