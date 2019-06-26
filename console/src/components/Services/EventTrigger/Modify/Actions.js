import defaultState from './State';
import { loadTriggers, makeMigrationCall, setTrigger } from '../EventActions';
import { UPDATE_MIGRATION_STATUS_ERROR } from '../../../Main/Actions';
import { showErrorNotification } from '../../Common/Notification';

import { MANUAL_TRIGGER_VAR } from './utils';

const SET_DEFAULTS = 'ModifyTrigger/SET_DEFAULTS';
export const setDefaults = () => ({ type: SET_DEFAULTS });

const SET_WEBHOOK_URL = 'ModifyTrigger/SET_WEBHOOK_URL';
const SET_WEBHOOK_URL_TYPE = 'ModifyTrigger/SET_WEBHOOK_URL_TYPE';
export const setWebhookUrl = data => ({ type: SET_WEBHOOK_URL, data });
export const setWebhookUrlType = data => ({ type: SET_WEBHOOK_URL_TYPE, data });

const SET_RETRY_NUM = 'ModifyTrigger/SET_RETRY_NUM';
const SET_RETRY_INTERVAL = 'ModifyTrigger/SET_RETRY_INTERVAL';
const SET_RETRY_TIMEOUT = 'ModifyTrigger/SET_RETRY_TIMEOUT';
export const setRetryNum = data => ({ type: SET_RETRY_NUM, data });
export const setRetryInterval = data => ({ type: SET_RETRY_INTERVAL, data });
export const setRetryTimeout = data => ({ type: SET_RETRY_TIMEOUT, data });

const TOGGLE_COLUMN = 'ModifyTrigger/TOGGLE_COLUMNS';
const TOGGLE_QUERY_TYPE = 'ModifyTrigger/TOGGLE_QUERY_TYPE_SELECTED';
const TOGGLE_MANUAL_QUERY_TYPE = 'ModifyTrigger/TOGGLE_MANUAL_QUERY_SELECTED';
export const RESET_MODIFY_STATE = 'ModifyTrigger/RESET_MODIFY_STATE';

export const toggleQueryType = ({ query, columns, value }) => ({
  type: TOGGLE_QUERY_TYPE,
  query,
  columns,
  value,
});
export const toggleManualType = ({ value }) => ({
  type: TOGGLE_MANUAL_QUERY_TYPE,
  data: value,
});
export const toggleColumn = (query, column) => ({
  type: TOGGLE_COLUMN,
  query,
  column,
});

const REMOVE_HEADER = 'ModifyTrigger/REMOVE_HEADER';
const SET_HEADERKEY = 'ModifyTrigger/SET_HEADERKEY';
const SET_HEADERTYPE = 'ModifyTrigger/SET_HEADERTYPE';
const SET_HEADERVALUE = 'ModifyTrigger/SET_HEADERVALUE';
const ADD_HEADER = 'ModifyTrigger/ADD_HEADER';
export const addHeader = () => ({ type: ADD_HEADER });
export const removeHeader = data => ({ type: REMOVE_HEADER, data });
export const setHeaderKey = (data, index) => ({
  type: SET_HEADERKEY,
  data,
  index,
});
export const setHeaderType = (data, index) => ({
  type: SET_HEADERTYPE,
  data,
  index,
});
export const setHeaderValue = (data, index) => ({
  type: SET_HEADERVALUE,
  data,
  index,
});

export const REQUEST_ONGOING = 'ModifyTrigger/REQUEST_ONGOING';
export const REQUEST_COMPLETE = 'ModifyTrigger/REQUEST_COMPLETE';

export const showValidationError = message => {
  return dispatch => {
    dispatch(
      showErrorNotification('Error modifying trigger!', 'Invalid input', {
        custom: message,
      })
    );
  };
};

export const save = (property, triggerName) => {
  return (dispatch, getState) => {
    const { modifyTrigger } = getState();
    const oldTrigger = getState().triggers.triggerList.find(
      tr => tr.name === triggerName
    );
    const downPayload = {
      replace: true,
      name: oldTrigger.name,
      table: {
        name: oldTrigger.table_name,
        schema: oldTrigger.table_schema,
      },
      retry_conf: { ...oldTrigger.configuration.retry_conf },
      ...oldTrigger.configuration.definition,
      headers: [...oldTrigger.configuration.headers],
    };
    if (oldTrigger.configuration.webhook_from_env) {
      downPayload.webhook_from_env = oldTrigger.configuration.webhook_from_env;
    } else {
      downPayload.webhook = oldTrigger.configuration.webhook;
    }
    const upPayload = {
      ...downPayload,
    };
    if (property === 'webhook') {
      if (modifyTrigger.webhookUrlType === 'env') {
        delete upPayload.webhook;
        upPayload.webhook_from_env = modifyTrigger.webhookURL;
      } else {
        delete upPayload.webhook_from_env;
        upPayload.webhook = modifyTrigger.webhookURL;
      }
    } else if (property === 'ops') {
      delete upPayload.update;
      delete upPayload.delete;
      delete upPayload.insert;
      upPayload.update = modifyTrigger.definition.update;
      upPayload.insert = modifyTrigger.definition.insert;
      upPayload.delete = modifyTrigger.definition.delete;
      // Add only if the value is true
      if (MANUAL_TRIGGER_VAR in modifyTrigger.definition) {
        delete upPayload[MANUAL_TRIGGER_VAR];
        upPayload[MANUAL_TRIGGER_VAR] =
          modifyTrigger.definition[MANUAL_TRIGGER_VAR];
      }
    } else if (property === 'retry') {
      upPayload.retry_conf = {
        num_retries: modifyTrigger.retryConf.numRetrys,
        interval_sec: modifyTrigger.retryConf.retryInterval,
        timeout_sec: modifyTrigger.retryConf.timeout,
      };
    } else if (property === 'headers') {
      delete upPayload.headers;
      upPayload.headers = [];
      modifyTrigger.headers
        .filter(h => Boolean(h.key.trim()))
        .forEach(h => {
          const { key, value, type } = h;
          if (type === 'env') {
            upPayload.headers.push({
              name: key.trim(),
              value_from_env: value.trim(),
            });
          } else {
            upPayload.headers.push({
              name: key.trim(),
              value: value.trim(),
            });
          }
        });
    }
    const upQuery = {
      type: 'bulk',
      args: [
        {
          type: 'create_event_trigger',
          args: {
            ...upPayload,
          },
        },
      ],
    };
    const downQuery = {
      type: 'bulk',
      args: [
        {
          type: 'create_event_trigger',
          args: {
            ...downPayload,
          },
        },
      ],
    };
    const migrationName = `modify_tr_${triggerName}_${property}`;
    const requestMsg = 'Updating trigger';
    const successMsg = 'Updated trigger';
    const errorMsg = 'Updating trigger failed';
    const customOnSuccess = () => {
      dispatch({ type: REQUEST_COMPLETE });
      dispatch(setTrigger(triggerName.trim()));
      dispatch(loadTriggers([triggerName]));
      return;
    };
    const customOnError = err => {
      dispatch({ type: REQUEST_COMPLETE });
      dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
      return;
    };
    dispatch({ type: REQUEST_ONGOING, data: property });
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

const reducer = (state = defaultState, action) => {
  switch (action.type) {
    case SET_WEBHOOK_URL:
      return {
        ...state,
        webhookURL: action.data,
      };
    case SET_WEBHOOK_URL_TYPE:
      return {
        ...state,
        webhookUrlType: action.data,
      };
    case TOGGLE_QUERY_TYPE:
      const newDefinition = { ...state.definition };
      if (action.value) {
        if (action.query === 'update') {
          newDefinition[action.query] = { columns: action.columns };
        } else {
          newDefinition[action.query] = { columns: '*' };
        }
      } else {
        delete newDefinition[action.query];
      }
      return {
        ...state,
        definition: newDefinition,
      };
    case TOGGLE_MANUAL_QUERY_TYPE:
      return {
        ...state,
        definition: {
          ...state.definition,
          [MANUAL_TRIGGER_VAR]: action.data,
        },
      };
    case TOGGLE_COLUMN:
      const queryColumns = [...state.definition[action.query].columns];
      if (queryColumns.find(qc => qc === action.column)) {
        return {
          ...state,
          definition: {
            ...state.definition,
            [action.query]: {
              columns: queryColumns.filter(qc => qc !== action.column),
            },
          },
        };
      }
      return {
        ...state,
        definition: {
          ...state.definition,
          [action.query]: { columns: [...queryColumns, action.column] },
        },
      };
    case SET_RETRY_NUM:
      return {
        ...state,
        retryConf: {
          ...state.retryConf,
          numRetrys: action.data,
        },
      };
    case SET_RETRY_INTERVAL:
      return {
        ...state,
        retryConf: {
          ...state.retryConf,
          retryInterval: action.data,
        },
      };
    case SET_RETRY_TIMEOUT:
      return {
        ...state,
        retryConf: {
          ...state.retryConf,
          timeout: action.data,
        },
      };
    case ADD_HEADER:
      return {
        ...state,
        headers: [...state.headers, { key: '', type: 'static', value: '' }],
      };
    case REMOVE_HEADER:
      return {
        ...state,
        headers: state.headers.filter((h, i) => i !== action.data),
      };
    case SET_HEADERKEY:
      const kNewHeaders = [...state.headers];
      kNewHeaders[action.index].key = action.data;
      return {
        ...state,
        headers: kNewHeaders,
      };
    case SET_HEADERVALUE:
      const vNewHeaders = [...state.headers];
      vNewHeaders[action.index].value = action.data;
      return {
        ...state,
        headers: vNewHeaders,
      };
    case SET_HEADERTYPE:
      const tNewHeaders = [...state.headers];
      tNewHeaders[action.index].type = action.data;
      return {
        ...state,
        headers: tNewHeaders,
      };
    case REQUEST_ONGOING:
      return {
        ...state,
        ongoingRequest: action.data,
      };
    case REQUEST_COMPLETE:
      return {
        ...state,
        ongoingRequest: null,
      };
    case RESET_MODIFY_STATE:
      return {
        ...defaultState,
      };
    default:
      return {
        ...state,
      };
  }
};

export default reducer;
