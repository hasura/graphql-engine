import defaultState from './State';

const LOAD_REQUEST = 'App/ONGOING_REQUEST';
const DONE_REQUEST = 'App/DONE_REQUEST';
const FAILED_REQUEST = 'App/FAILED_REQUEST';
const ERROR_REQUEST = 'App/ERROR_REQUEST';
const CONNECTION_FAILED = 'App/CONNECTION_FAILED';
const CLOSE_MODAL = 'App/CLOSE_MODAL';
const NOTIF_EXPANDED = 'App/NOTIF_EXPANDED';
const NOTIF_MSG = 'App/NOTIF_MSG';

const notifExpand = isExpanded => ({ type: NOTIF_EXPANDED, data: isExpanded });
const notifMsg = finalMsg => ({ type: NOTIF_MSG, data: finalMsg });

const progressBarReducer = (state = defaultState, action) => {
  switch (action.type) {
    case LOAD_REQUEST:
      return {
        ...state,
        ongoingRequest: true,
        percent: 10,
        requestSuccess: null,
        requestError: null,
        connectionFailed: false,
      };

    case DONE_REQUEST:
      return {
        ...state,
        percent: 100,
        ongoingRequest: false,
        requestSuccess: true,
        requestError: null,
        connectionFailed: false,
      };

    case FAILED_REQUEST:
      return {
        ...state,
        percent: 100,
        ongoingRequest: false,
        requestSuccess: null,
        requestError: true,
        connectionFailed: false,
      };

    case ERROR_REQUEST:
      return {
        ...state,
        modalOpen: true,
        error: action.data,
        reqURL: action.url,
        reqData: action.params,
        statusCode: action.statusCode,
        connectionFailed: false,
      };
    case CONNECTION_FAILED:
      return {
        ...state,
        modalOpen: true,
        error: true,
        connectionFailed: true,
      };

    case CLOSE_MODAL:
      return {
        ...state,
        modalOpen: false,
      };

    case NOTIF_EXPANDED:
      return {
        ...state,
        isNotifExpanded: action.data,
      };
    case NOTIF_MSG:
      return {
        ...state,
        notifMsg: action.data,
      };

    default:
      return state;
  }
};

export default progressBarReducer;
export {
  LOAD_REQUEST,
  DONE_REQUEST,
  FAILED_REQUEST,
  ERROR_REQUEST,
  CLOSE_MODAL,
  CONNECTION_FAILED,
  NOTIF_EXPANDED,
  notifExpand,
  notifMsg,
};
