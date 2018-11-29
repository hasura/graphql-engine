import defaultState from './State';

const SET_WEBHOOK_URL = 'ModifyTrigger/SET_WEBHOOK_URL';
const SET_WEBHOOK_URL_TYPE = 'ModifyTrigger/SET_WEBHOOK_URL_TYPE';
export const setWebhookUrl = data => ({ type: SET_WEBHOOK_URL, data });
export const setWebhookUrlType = data => ({ type: SET_WEBHOOK_URL_TYPE, data });

const SET_RETRY_NUM = 'ModifyTrigger/SET_RETRY_NUM';
const SET_RETRY_INTERVAL = 'ModifyTrigger/SET_RETRY_INTERVAL';
export const setRetryNum = data => ({ type: SET_RETRY_NUM, data });
export const setRetryInterval = data => ({ type: SET_RETRY_INTERVAL, data });

const TOGGLE_COLUMN = 'ModifyTrigger/TOGGLE_COLUMNS';
const TOGGLE_QUERY_TYPE = 'ModifyTrigger/TOGGLE_QUERY_TYPE_SELECTED';
export const toggleQueryType = (query, columns) => ({
  type: TOGGLE_QUERY_TYPE,
  query,
  columns,
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

// const MAKING_REQUEST = 'ModifyTrigger/MAKING_REQUEST';
// const REQUEST_SUCCESS = 'ModifyTrigger/REQUEST_SUCCESS';
// const REQUEST_ERROR = 'ModifyTrigger/REQUEST_ERROR';
// const VALIDATION_ERROR = 'ModifyTrigger/VALIDATION_ERROR';

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
      if (newDefinition[action.query] !== undefined) {
        delete newDefinition[action.query];
      } else {
        newDefinition[action.query] = { columns: action.columns };
      }
      return {
        ...state,
        definition: newDefinition,
      };
    case TOGGLE_COLUMN:
      const queryColumns = [...state.definition[action.query].columns];
      console.log(queryColumns);
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
    default:
      return {
        ...defaultState,
      };
  }
};

export default reducer;
