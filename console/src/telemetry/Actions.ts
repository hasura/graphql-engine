import { Dispatch, AnyAction } from 'redux';
import { ThunkDispatch } from 'redux-thunk';
import Endpoints, { globalCookiePolicy } from '../Endpoints';
import requestAction from '../utils/requestAction';
import dataHeaders from '../components/Services/Data/Common/Headers';
import {
  getRunSqlQuery,
  getConsoleOptsQuery,
} from '../components/Common/utils/v1QueryUtils';
import {
  showErrorNotification,
  showSuccessNotification,
} from '../components/Services/Common/Notification';
import globals from '../Globals';
import defaultTelemetryState, { TelemetryState } from './state';
import { GetReduxState, ReduxState } from '../types';

const SET_CONSOLE_OPTS = 'Telemetry/SET_CONSOLE_OPTS';
const SET_NOTIFICATION_SHOWN = 'Telemetry/SET_NOTIFICATION_SHOWN';
const SET_HASURA_UUID = 'Telemetry/SET_HASURA_UUID';

type Telemetry = {
  console_state: TelemetryState['console_opts'];
  hasura_uuid: string;
};

const setConsoleOptsInDB = (
  opts: TelemetryState['console_opts'],
  successCb: (arg: object) => void,
  errorCb: (arg: Error) => void
) => (
  dispatch: ThunkDispatch<ReduxState, {}, AnyAction>,
  getState: GetReduxState
) => {
  const url = Endpoints.getSchema;

  const { hasura_uuid, console_opts } = getState().telemetry;

  const consoleState = {
    ...console_opts,
    ...opts,
  };

  if (!hasura_uuid) {
    dispatch(
      showErrorNotification(
        'Opt out of pre-release notifications failed',
        'Internal error: missing hasura_uuid'
      )
    );
    return;
  }

  const options: RequestInit = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(
      getRunSqlQuery(
        `update hdb_catalog.hdb_version set console_state = '${JSON.stringify(
          consoleState
        )}' where hasura_uuid='${hasura_uuid}';`
      )
    ),
  };

  // eslint-disable-next-line consistent-return
  return dispatch(requestAction(url, options)).then(
    (data: object) => {
      if (successCb) {
        successCb(data);
      }
    },
    (error: Error) => {
      if (errorCb) {
        errorCb(error);
      }
    }
  );
};

const telemetryNotificationShown = () => (
  dispatch: Dispatch<TelemetryActionTypes>
) => {
  dispatch({ type: SET_NOTIFICATION_SHOWN });
};

const setTelemetryNotificationShownInDB = () => {
  const successCb = (data: object) => {
    console.log(
      `Updated telemetry notification status in db ${JSON.stringify(data)}`
    );
  };

  const errorCb = (error: Error) => {
    console.error(
      `Failed to update telemetry notification status in db ${JSON.stringify(
        error
      )}`
    );
  };

  const opts = {
    telemetryNotificationShown: true,
  };

  return setConsoleOptsInDB(opts, successCb, errorCb);
};

const setPreReleaseNotificationOptOutInDB = () => (
  dispatch: ThunkDispatch<ReduxState, {}, AnyAction>
) => {
  const successCb = () => {
    dispatch(
      showSuccessNotification(
        'Success',
        'Opted out of pre-release version release notifications'
      )
    );
  };

  const errorCb = (error: Error) => {
    dispatch(showErrorNotification('Failed to opt out', null, error));
  };

  const options = {
    disablePreReleaseUpdateNotifications: true,
  };

  return dispatch(setConsoleOptsInDB(options, successCb, errorCb));
};

const loadConsoleOpts = () => {
  return (
    dispatch: ThunkDispatch<ReduxState, {}, AnyAction>,
    getState: GetReduxState
  ) => {
    const url = Endpoints.getSchema;
    const options: RequestInit = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(getConsoleOptsQuery()),
    };

    return dispatch(requestAction(url, options) as any).then(
      (data: Telemetry[]) => {
        if (data.length !== 0) {
          dispatch({
            type: SET_HASURA_UUID,
            data: data[0].hasura_uuid,
          });
          globals.hasuraUUID = data[0].hasura_uuid;
          dispatch({
            type: SET_CONSOLE_OPTS,
            data: data[0].console_state,
          });
          globals.telemetryNotificationShown = !!data[0].console_state
            ?.telemetryNotificationShown;
        }
        return Promise.resolve();
      },
      (error: Error) => {
        console.error(
          `Failed to load console options: ${JSON.stringify(error)}`
        );
        return Promise.reject();
      }
    );
  };
};

interface SetConsoleOptsAction {
  type: typeof SET_CONSOLE_OPTS;
  data: object;
}

interface SetNotificationShowAction {
  type: typeof SET_NOTIFICATION_SHOWN;
  data?: object;
}

interface SetHasuraUuid {
  type: typeof SET_HASURA_UUID;
  data: string;
}

type TelemetryActionTypes =
  | SetConsoleOptsAction
  | SetNotificationShowAction
  | SetHasuraUuid;

export const requireConsoleOpts = ({
  dispatch,
}: {
  dispatch: ThunkDispatch<ReduxState, {}, AnyAction>;
}) => (nextState: ReduxState, replaceState: ReduxState, callback: any) => {
  dispatch(loadConsoleOpts()).finally(callback);
};

const telemetryReducer = (
  state = defaultTelemetryState,
  action: TelemetryActionTypes
) => {
  switch (action.type) {
    case SET_CONSOLE_OPTS:
      return {
        ...state,
        console_opts: {
          ...action.data,
        },
      };
    case SET_NOTIFICATION_SHOWN:
      return {
        ...state,
        console_opts: {
          ...state.console_opts,
          telemetryNotificationShown: true,
        },
      };
    case SET_HASURA_UUID:
      return {
        ...state,
        hasura_uuid: action.data,
      };
    default:
      return state;
  }
};

export default telemetryReducer;
export {
  setConsoleOptsInDB,
  loadConsoleOpts,
  telemetryNotificationShown,
  setPreReleaseNotificationOptOutInDB,
  setTelemetryNotificationShownInDB,
};
