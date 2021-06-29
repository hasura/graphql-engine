import { Middleware } from 'redux';
import endpoints from '../Endpoints';
import globals from '../Globals';
import { filterEventsBlockList, sanitiseUrl } from './filters';
import { RUN_TIME_ERROR } from '../components/Main/Actions';
import { REDUX_LOCATION_CHANGE_ACTION_TYPE } from '../constants';
import { GetReduxState } from '../types';

interface TelemetryGlobals {
  serverVersion: string;
  consoleMode: string;
  cliUUID: string;
  hasuraUUID: string;
}

export type ErrorPayload = {
  message: string;
  stack?: string;
};

export type TelemetryAction =
  | {
      type: typeof REDUX_LOCATION_CHANGE_ACTION_TYPE;
      payload: {
        pathname: string;
      };
    }
  | {
      type: typeof RUN_TIME_ERROR;
      data?: ErrorPayload;
    };

export type TelemetryPayload = {
  server_version: string;
  event_type: string;
  url: string;
  console_mode: string;
  cli_uuid: string;
  server_uuid: string;
  data?: ErrorPayload;
};

const createClient = () => {
  if (globals.enableTelemetry) {
    try {
      const client = new WebSocket(endpoints.telemetryServer);
      client.onerror = e => {
        console.error(`WebSocket Error for Events${e}`);
      };
      return client;
    } catch (e) {
      console.error('Unable to initialise telemetry client', e);
      return null;
    }
  }
  return null;
};

let client = createClient();
if (client) {
  const onClose = () => {
    client = createClient();
    if (client) {
      client.onclose = onClose;
    }
  };
  client.onclose = onClose;
}

const isTelemetryConnectionReady = () => {
  return !!(client && client.readyState === client.OPEN);
};

const sendEvent = (payload: TelemetryPayload) => {
  if (client && isTelemetryConnectionReady()) {
    client.send(
      JSON.stringify({ data: payload, topic: globals.telemetryTopic })
    );
  }
};

export const trackReduxAction = (
  action: TelemetryAction,
  getState: GetReduxState
) => {
  const actionType = action.type;
  // filter events
  if (!filterEventsBlockList.includes(actionType)) {
    const serverVersion = getState().main.serverVersion;
    const url = sanitiseUrl(window.location.pathname);

    const reqBody: TelemetryPayload = {
      server_version: serverVersion,
      event_type: actionType,
      url,
      console_mode: globals.consoleMode,
      cli_uuid: globals.cliUUID,
      server_uuid: getState().telemetry.hasura_uuid,
    };

    if (action.type === REDUX_LOCATION_CHANGE_ACTION_TYPE) {
      // capture page views
      const payload = action.payload;
      reqBody.url = sanitiseUrl(payload.pathname);
    }

    if (action.type === RUN_TIME_ERROR) {
      reqBody.data = action.data;
    }

    // Send the data
    sendEvent(reqBody);
  }
};

export const trackRuntimeError = (
  telemeteryGlobals: TelemetryGlobals,
  error: Error
) => {
  const reqBody: TelemetryPayload = {
    server_version: telemeteryGlobals.serverVersion,
    event_type: RUN_TIME_ERROR,
    url: sanitiseUrl(window.location.pathname),
    console_mode: telemeteryGlobals.consoleMode,
    cli_uuid: telemeteryGlobals.cliUUID,
    server_uuid: telemeteryGlobals.hasuraUUID,
    data: { message: error.message, stack: error.stack },
  };
  sendEvent(reqBody);
};

export const telemetryMiddleware: Middleware = ({
  getState,
}) => next => action => {
  // Call the next dispatch method in the middleware chain.
  const returnValue = next(action);

  // check if analytics tracking is enabled
  if (globals.enableTelemetry) {
    trackReduxAction(action, getState);
  }
  // This will likely be the action itself, unless
  // a middleware further in chain changed it.
  return returnValue;
};
