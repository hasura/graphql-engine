import Endpoints, { globalCookiePolicy } from '../Endpoints';
import requestAction from '../utils/requestAction';
import dataHeaders from '../components/Services/Data/Common/Headers';
import defaultTelemetryState from './State';
import {
  getRunSqlQuery,
  getConsoleOptsQuery,
} from '../components/Common/utils/v1QueryUtils';
import {
  showErrorNotification,
  showSuccessNotification,
} from '../components/Services/Common/Notification';
import globals from '../Globals';

const SET_CONSOLE_OPTS = 'Telemetry/SET_CONSOLE_OPTS';
const SET_NOTIFICATION_SHOWN = 'Telemetry/SET_NOTIFICATION_SHOWN';
const SET_HASURA_UUID = 'Telemetry/SET_HASURA_UUID';

const setConsoleOptsInDB = (opts, successCb, errorCb) => (
  dispatch,
  getState
) => {
  const url = Endpoints.getSchema;

  const { hasura_uuid, console_opts } = getState().telemetry;

  const consoleState = {
    ...console_opts,
    ...opts,
  };

  if (!hasura_uuid) {
    if (errorCb) {
      const err = new Error('Internal Server Occurred!');
      errorCb(err);
      return;
    }
    // exit action, as callback isn't available
    return;
  }

  const options = {
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

  return dispatch(requestAction(url, options)).then(
    data => {
      if (successCb) {
        successCb(data);
      }
    },
    error => {
      if (errorCb) {
        errorCb(error);
      }
    }
  );
};

const telemetryNotificationShown = () => dispatch => {
  dispatch({ type: SET_NOTIFICATION_SHOWN });
};

const setTelemetryNotificationShownInDB = () => {
  const successCb = data => {
    console.log(
      'Updated telemetry notification status in db' + JSON.stringify(data)
    );
  };

  const errorCb = error => {
    console.error(
      'Failed to update telemetry notification status in db' +
        JSON.stringify(error)
    );
  };

  const opts = {
    telemetryNotificationShown: true,
  };

  return setConsoleOptsInDB(opts, successCb, errorCb);
};

const setPreReleaseNotificationOptOutInDB = () => dispatch => {
  const successCb = () => {
    dispatch(
      showSuccessNotification(
        'Success',
        'Opted out of pre-release version release notifications'
      )
    );
  };

  const errorCb = error => {
    dispatch(showErrorNotification('Failed to opt out', null, error));
  };

  const opts = {
    disablePreReleaseUpdateNotifications: true,
  };

  return dispatch(setConsoleOptsInDB(opts, successCb, errorCb));
};

export const loadConsoleOpts = () => {
  return (dispatch, getState) => {
    const url = Endpoints.getSchema;
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(getConsoleOptsQuery()),
    };

    return dispatch(requestAction(url, options)).then(
      data => {
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
          globals.telemetryNotificationShown =
            data[0].console_state.telemetryNotificationShown;
        }
        return Promise.resolve();
      },
      error => {
        console.error(
          'Failed to load console options: ' + JSON.stringify(error)
        );
        return Promise.reject();
      }
    );
  };
};

export const requireConsoleOpts = ({ dispatch }) => (
  nextState,
  replaceState,
  callback
) => {
  dispatch(loadConsoleOpts()).finally(callback);
};

const telemetryReducer = (state = defaultTelemetryState, action) => {
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
  telemetryNotificationShown,
  setPreReleaseNotificationOptOutInDB,
  setTelemetryNotificationShownInDB,
};
