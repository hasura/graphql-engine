import Endpoints, { globalCookiePolicy } from '../Endpoints';
import requestAction from '../utils/requestAction';
import dataHeaders from '../components/Services/Data/Common/Headers';
import defaultTelemetryState from './State';

const SET_MISC_OPTS = 'Telemetry/SET_MISC_OPTS';
const SET_NOTIFICATION_SHOWN = 'Telemetry/SET_NOTIFICATION_SHOWN';
const SET_HASURA_UUID = 'Telemetry/SET_HASURA_UUID';

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
        sql: `update hdb_catalog.hdb_version set misc_state = misc_state::jsonb || jsonb_build_object('console', ( select coalesce(misc_state->'console', '{}'::jsonb)||'{"telemetryNotificationShown":true}'::jsonb from hdb_catalog.hdb_version)) where hasura_uuid='${uuid}';`,
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

const loadMiscOpts = () => (dispatch, getState) => {
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
        columns: ['hasura_uuid', 'misc_state'],
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
          type: SET_MISC_OPTS,
          data: data[0].misc_state,
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

const telemetryReducer = (state = defaultTelemetryState, action) => {
  switch (action.type) {
    case SET_MISC_OPTS:
      if (action.data.console) {
        if (
          typeof action.data.console.telemetryNotificationShown === 'undefined'
        ) {
          action.data.console = {
            ...action.data.console,
            telemetryNotificationShown: false,
          };
        }
      } else {
        action.data = {
          ...action.data,
          console: {
            telemetryNotificationShown: false,
          },
        };
      }
      return {
        ...state,
        misc_opts: action.data,
      };
    case SET_NOTIFICATION_SHOWN:
      return {
        ...state,
        misc_opts: {
          ...state.misc_opts,
          console: {
            ...state.misc_opts.console,
            telemetryNotificationShown: true,
          },
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
export { loadMiscOpts, telemetryNotificationShown, setNotificationShownInDB };
