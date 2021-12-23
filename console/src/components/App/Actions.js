import defaultState from './State';
import { loadConsoleOpts } from '../../telemetry/Actions';
import { fetchServerConfig, fetchHerokuSession } from '../Main/Actions';

const LOAD_REQUEST = 'App/ONGOING_REQUEST';
const DONE_REQUEST = 'App/DONE_REQUEST';
const FAILED_REQUEST = 'App/FAILED_REQUEST';
const ERROR_REQUEST = 'App/ERROR_REQUEST';
const CONNECTION_FAILED = 'App/CONNECTION_FAILED';

export const requireAsyncGlobals = (
  { dispatch },
  shouldLoadOpts = true,
  shouldLoadServerConfig = true
) => {
  return (nextState, finalState, callback) => {
    Promise.all([
      shouldLoadOpts && dispatch(loadConsoleOpts()),
      shouldLoadServerConfig && dispatch(fetchServerConfig),
      dispatch(fetchHerokuSession()),
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
