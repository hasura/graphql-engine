import { Dispatch, AnyAction } from 'redux';
import { ThunkDispatch } from 'redux-thunk';
import Endpoints, { globalCookiePolicy } from '../Endpoints';
import requestAction from '../utils/requestAction';
import dataHeaders from '../components/Services/Data/Common/Headers';
import {
  getRunSqlQuery,
  getConsoleOptsQuery,
  getUpdateConsoleStateQuery,
} from '../components/Common/utils/v1QueryUtils';
import {
  showErrorNotification,
  showSuccessNotification,
} from '../components/Services/Common/Notification';
import globals from '../Globals';
import defaultTelemetryState from './state';
import {
  GetReduxState,
  ReduxState,
  ConsoleState,
  NotificationsState,
} from '../types';
import { isUpdateIDsEqual } from './utils';
import { HASURA_COLLABORATOR_TOKEN } from '../constants';
import { getUserType } from '../components/Main/utils';

const SET_CONSOLE_OPTS = 'Telemetry/SET_CONSOLE_OPTS';
const SET_NOTIFICATION_SHOWN = 'Telemetry/SET_NOTIFICATION_SHOWN';
const SET_HASURA_UUID = 'Telemetry/SET_HASURA_UUID';
const UPDATE_CONSOLE_NOTIFICATIONS = 'Telemetry/UPDATE_CONSOLE_NOTIFICATIONS';

type Telemetry = {
  console_state: ConsoleState['console_opts'];
  hasura_uuid: string;
};

const setConsoleOptsInDB = (
  opts: ConsoleState['console_opts'],
  successCb: (arg: Record<string, any>) => void,
  errorCb: (arg: Error) => void
) => (
  dispatch: ThunkDispatch<ReduxState, unknown, AnyAction>,
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
    (data: Record<string, unknown>) => {
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
  const successCb = (data: Record<string, unknown>) => {
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
  dispatch: ThunkDispatch<ReduxState, unknown, AnyAction>,
  getState: GetReduxState
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

  dispatch({
    type: SET_CONSOLE_OPTS,
    data: {
      ...getState().telemetry.console_opts,
      disablePreReleaseUpdateNotifications: true,
    },
  });

  return dispatch(setConsoleOptsInDB(options, successCb, errorCb));
};

const updateConsoleNotificationsState = (updatedState: NotificationsState) => {
  return (
    dispatch: ThunkDispatch<ReduxState, unknown, AnyAction>,
    getState: GetReduxState
  ) => {
    const url = Endpoints.schemaChange;
    const currentNotifications = getState().main.consoleNotifications;
    const restState = getState().telemetry.console_opts;
    const headers = dataHeaders(getState);
    let userType = 'admin';
    if (headers?.[HASURA_COLLABORATOR_TOKEN]) {
      const collabToken = headers[HASURA_COLLABORATOR_TOKEN];
      userType = getUserType(collabToken);
    }
    let composedUpdatedState: ConsoleState['console_opts'] = {
      ...restState,
      console_notifications: {
        [userType]: updatedState,
      },
    };
    if (userType !== 'admin') {
      const currentState = restState?.console_notifications;
      if (Object.keys(currentState ?? {}).length > 1) {
        composedUpdatedState = {
          ...restState,
          console_notifications: {
            ...currentState,
            [userType]: updatedState,
          },
        };
      }
    }
    if (currentNotifications && Array.isArray(currentNotifications)) {
      if (isUpdateIDsEqual(currentNotifications, updatedState.read)) {
        composedUpdatedState = {
          ...restState,
          console_notifications: {
            ...restState?.console_notifications,
            [userType]: {
              read: 'all',
              showBadge: false,
              date: updatedState.date,
            },
          },
        };
        // update the localStorage var with all the notifications
        // since all the notifications were clicked on read state
        window.localStorage.setItem(
          'notifications:data',
          JSON.stringify(currentNotifications)
        );
      }
    }
    const updatedReadNotifications = getUpdateConsoleStateQuery(
      composedUpdatedState
    );
    const options: RequestInit = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers,
      body: JSON.stringify(updatedReadNotifications),
    };
    return dispatch(requestAction(url, options))
      .then((data: any) => {
        dispatch({
          type: UPDATE_CONSOLE_NOTIFICATIONS,
          data: data.returning[0].console_state.console_notifications,
        });
      })
      .catch(error => {
        console.error(
          'There was an error in updating the console notifications.',
          error
        );
        return error;
      });
  };
};

const loadConsoleOpts = () => {
  return (
    dispatch: ThunkDispatch<ReduxState, unknown, AnyAction>,
    getState: GetReduxState
  ) => {
    const url = Endpoints.getSchema;
    const headers = dataHeaders(getState);
    const options: RequestInit = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers,
      body: JSON.stringify(getConsoleOptsQuery()),
    };
    let userType = 'admin';
    if (headers?.[HASURA_COLLABORATOR_TOKEN]) {
      userType = headers[HASURA_COLLABORATOR_TOKEN];
    }
    return dispatch(requestAction(url, options) as any).then(
      (data: Telemetry[]) => {
        if (data.length) {
          const { hasura_uuid, console_state } = data[0];

          dispatch({
            type: SET_HASURA_UUID,
            data: hasura_uuid,
          });
          globals.hasuraUUID = hasura_uuid;

          dispatch({
            type: SET_CONSOLE_OPTS,
            data: console_state,
          });

          globals.telemetryNotificationShown = !!console_state?.telemetryNotificationShown;

          if (!console_state?.console_notifications) {
            dispatch({
              type: UPDATE_CONSOLE_NOTIFICATIONS,
              data: {
                [userType]: {
                  read: [],
                  date: null,
                  showBadge: true,
                },
              },
            });
          }

          return Promise.resolve();
        }
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
  data: Record<string, unknown>;
}

interface SetNotificationShowAction {
  type: typeof SET_NOTIFICATION_SHOWN;
  data?: Record<string, unknown>;
}

interface SetHasuraUuid {
  type: typeof SET_HASURA_UUID;
  data: string;
}

interface UpdateConsoleNotifications {
  type: typeof UPDATE_CONSOLE_NOTIFICATIONS;
  data: Record<string, any>;
}

type TelemetryActionTypes =
  | SetConsoleOptsAction
  | SetNotificationShowAction
  | SetHasuraUuid
  | UpdateConsoleNotifications;

export const requireConsoleOpts = ({
  dispatch,
}: {
  dispatch: ThunkDispatch<ReduxState, unknown, AnyAction>;
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
    case UPDATE_CONSOLE_NOTIFICATIONS:
      return {
        ...state,
        console_opts: {
          ...state.console_opts,
          console_notifications: action.data,
        },
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
  updateConsoleNotificationsState,
};
