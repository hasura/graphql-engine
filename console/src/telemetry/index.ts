import endpoints from '../Endpoints';
import globals from '../Globals';
import { filterEventsBlockList, sanitiseUrl } from './filters';
import { RUN_TIME_ERROR } from '../components/Main/Actions';

type ReduxAction = {
  type: string;
  payload: {
    pathname: string;
  };
  data: any;
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

const sendEvent = (payload: any) => {
  if (client && isTelemetryConnectionReady()) {
    client.send(
      JSON.stringify({ data: payload, topic: globals.telemetryTopic })
    );
  }
};

export const trackReduxAction = (action: ReduxAction, getState: any) => {
  const actionType = action.type;
  // filter events
  if (!filterEventsBlockList.includes(actionType)) {
    const serverVersion = getState().main.serverVersion;
    const url = sanitiseUrl(window.location.pathname);

    const reqBody = {
      server_version: serverVersion,
      event_type: actionType,
      url,
      console_mode: globals.consoleMode,
      cli_uuid: globals.cliUUID,
      server_uuid: getState().telemetry.hasura_uuid,
      data: null,
    };

    const isLocationType = actionType === '@@router/LOCATION_CHANGE';
    if (isLocationType) {
      // capture page views
      const payload = action.payload;
      reqBody.url = sanitiseUrl(payload.pathname);
    }

    const isErrorType = actionType === RUN_TIME_ERROR;
    if (isErrorType) {
      reqBody.data = action.data;
    }

    // Send the data
    sendEvent(reqBody);
  }
};

export const trackRuntimeError = (telemeteryGlobals: any, data: any) => {
  const reqBody = {
    server_version: telemeteryGlobals.serverVersion,
    event_type: RUN_TIME_ERROR,
    url: sanitiseUrl(window.location.pathname),
    console_mode: telemeteryGlobals.consoleMode,
    cli_uuid: telemeteryGlobals.cliUUID,
    server_uuid: telemeteryGlobals.hasuraUUID,
    data,
  };
  sendEvent(reqBody);
};
