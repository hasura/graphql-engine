import defaultState from './State';
import { loadConsoleOpts } from '../../telemetry/Actions';
import { fetchServerConfig } from '../Main/Actions';

const LOAD_REQUEST = 'App/ONGOING_REQUEST';
const DONE_REQUEST = 'App/DONE_REQUEST';
const FAILED_REQUEST = 'App/FAILED_REQUEST';
const ERROR_REQUEST = 'App/ERROR_REQUEST';
const CONNECTION_FAILED = 'App/CONNECTION_FAILED';

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

export const requireAsyncGlobals = ({ dispatch }) => {
  return (nextState, finalState, callback) => {
    Promise.all([
      dispatch(loadConsoleOpts()),
      dispatch(fetchServerConfig()),
    ]).finally(callback);
  };
};

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
        ongoingRequest: false,
        connectionFailed: true,
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
  CONNECTION_FAILED,
};
