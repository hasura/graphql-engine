import { ThunkDispatch } from 'redux-thunk';

import defaultState from './State';
import { loadConsoleOpts } from '../../telemetry/Actions';
import { fetchServerConfig } from '../Main/Actions';
import { ReduxAction, ReduxState } from '../../types';

export const LOAD_REQUEST = 'App/ONGOING_REQUEST';
export const DONE_REQUEST = 'App/DONE_REQUEST';
export const FAILED_REQUEST = 'App/FAILED_REQUEST';
export const ERROR_REQUEST = 'App/ERROR_REQUEST';
export const CONNECTION_FAILED = 'App/CONNECTION_FAILED';

export const requireAsyncGlobals = ({
  dispatch,
}: {
  dispatch: ThunkDispatch<ReduxState, {}, ReduxAction>;
}) => {
  return (_nextState: unknown, _finalState: unknown, callback: () => void) => {
    Promise.all([
      dispatch(loadConsoleOpts),
      dispatch(fetchServerConfig()),
    ]).finally(callback);
  };
};

export type ProgressBarAction =
  | {
      type:
        | typeof LOAD_REQUEST
        | typeof DONE_REQUEST
        | typeof FAILED_REQUEST
        | typeof CONNECTION_FAILED;
    }
  | {
      type: typeof ERROR_REQUEST;
      data: string;
      url: string;
      params: string;
      statusCode: number;
    };

const progressBarReducer = (
  state = defaultState,
  action: ProgressBarAction
) => {
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
