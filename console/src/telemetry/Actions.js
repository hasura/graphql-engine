import Endpoints, { globalCookiePolicy } from '../Endpoints';
import requestAction from '../utils/requestAction';
import dataHeaders from '../components/Services/Data/Common/Headers';
import defaultTelemetryState from './State';

const SET_CONSOLE_OPTS = 'Telemetry/SET_CONSOLE_OPTS';
const SET_NOTIFICATION_SHOWN = 'Telemetry/SET_NOTIFICATION_SHOWN';
const SET_HASURA_UUID = 'Telemetry/SET_HASURA_UUID';
const SET_TELEMETRY_DISABLED = 'Telemetry/SET_TELEMETRY_DISABLED';

const telemetryNotificationShown = () => dispatch => {
  dispatch({ type: SET_NOTIFICATION_SHOWN });
};

const setNotificationShownInDB = () => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const uuid = getState().telemetry.hasura_uuid;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify({
      type: 'run_sql',
      args: {
        sql: `update hdb_catalog.hdb_version set console_state = console_state || jsonb_build_object('telemetryNotificationShown', true) where hasura_uuid='${uuid}';`,
      },
    }),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      console.log(
        'Updated telemetry notification status in db' + JSON.stringify(data)
      );
    },
    error => {
      console.error(
        'Failed to update telemetry notification status in db' +
          JSON.stringify(error)
      );
    }
  );
};

const loadConsoleTelemetryOpts = () => {
  return (dispatch, getState) => {
    if (window.__env.enableTelemetry === undefined) {
      return dispatch({ type: SET_TELEMETRY_DISABLED });
    }

    const url = Endpoints.getSchema;
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify({
        type: 'select',
        args: {
          table: {
            name: 'hdb_version',
            schema: 'hdb_catalog',
          },
          columns: ['hasura_uuid', 'console_state'],
        },
      }),
    };

    return dispatch(requestAction(url, options)).then(
      data => {
        if (data.length !== 0) {
          dispatch({
            type: SET_HASURA_UUID,
            data: data[0].hasura_uuid,
          });
          dispatch({
            type: SET_CONSOLE_OPTS,
            data: data[0].console_state,
          });
        }
      },
      error => {
        console.error(
          'Failed to load telemetry misc options' + JSON.stringify(error)
        );
      }
    );
  };
};

const telemetryReducer = (state = defaultTelemetryState, action) => {
  switch (action.type) {
    case SET_CONSOLE_OPTS:
      return {
        ...state,
        console_opts: {
          ...action.data,
          telemetryNotificationShown: action.data.telemetryNotificationShown
            ? true
            : false,
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
  loadConsoleTelemetryOpts,
  telemetryNotificationShown,
  setNotificationShownInDB,
};
