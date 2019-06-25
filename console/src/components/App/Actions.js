import defaultState from './State';
import jsonFormat from './JSONFormat';
import Notifications from 'react-notification-system-redux';

const LOAD_REQUEST = 'App/ONGOING_REQUEST';
const DONE_REQUEST = 'App/DONE_REQUEST';
const FAILED_REQUEST = 'App/FAILED_REQUEST';
const ERROR_REQUEST = 'App/ERROR_REQUEST';
const CONNECTION_FAILED = 'App/CONNECTION_FAILED';
const CLOSE_MODAL = 'App/CLOSE_MODAL';
const NOTIF_EXPANDED = 'App/NOTIF_EXPANDED';
const NOTIF_MSG = 'App/NOTIF_MSG';

/**
 * Global notification function
 * options: type default, description
 * level: string info, {success, error, warning, info}
 * position: string br, {tr, tl, tc, br, bl, bc}
 * title: string null
 * message: string null
 * autoDismiss: integer 5, set to 0 to not auto-dismiss
 * dismissible: bool true, set if user can dismiss notification
 * action: object null, action button with label string and callback function
 * children: element/string, null, add custom element, over-rides action
 * onAdd: function, null, called when notification is successfully created, 1st argument is the notification
 * onRemove: function, null, same as onAdd
 * uid: integer/string, null, unique identifier to the notification, same uid will not be shown again
 */
const showNotification = ({
  level = 'info',
  position = 'tr',
  json,
  ...options
} = {}) => {
  return dispatch => {
    if (level === 'success') {
      dispatch(Notifications.removeAll());
    }
    dispatch(
      Notifications.show(
        {
          position,
          autoDismiss: ['error', 'warning'].includes(level) ? 0 : 5,
          dismissible: ['error', 'warning'].includes(level) ? 'button' : 'both',
          children: json ? jsonFormat(json) : null,
          ...options,
        },
        level
      )
    );
  };
};

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
  showNotification,
};
