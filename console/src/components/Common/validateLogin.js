import {
  loadAdminSecretState,
  clearAdminSecretState,
  CONSOLE_ADMIN_SECRET,
} from '../AppState';
import globals from '../../Globals';
import Endpoints, { globalCookiePolicy } from '../../Endpoints';

import requestAction from '../../utils/requestActionPlain';

import { UPDATE_DATA_HEADERS } from '../Services/Data/DataActions';
import { changeRequestHeader } from '../ApiExplorer/Actions';

import { SERVER_CONSOLE_MODE } from '../../constants';

const checkValidity = adminSecret => {
  return dispatch => {
    const url = Endpoints.getSchema;
    const currentSchema = 'public';
    const headers = {
      'content-type': 'application/json',
      [`x-hasura-${globals.adminSecretName}`]: adminSecret,
    };
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: headers,
      body: JSON.stringify({
        type: 'select',
        args: {
          table: {
            name: 'hdb_table',
            schema: 'hdb_catalog',
          },
          columns: ['table_schema'],
          where: { table_schema: currentSchema },
          limit: 1,
        },
      }),
    };
    return dispatch(requestAction(url, options));
  };
};

const validateLogin = ({ dispatch }) => {
  return (nextState, replaceState, cb) => {
    // Validate isAdminSecretSet env is set by server or adminSecret env is set by cli
    if (globals.isAdminSecretSet || globals.adminSecret) {
      let adminSecret = '';
      // Check the console mode and retrieve adminSecret accordingly.
      if (globals.consoleMode === SERVER_CONSOLE_MODE) {
        adminSecret = loadAdminSecretState(CONSOLE_ADMIN_SECRET);
      } else {
        adminSecret = globals.adminSecret;
      }
      dispatch(checkValidity(adminSecret))
        .then(() => {
          return Promise.all([
            dispatch({
              type: UPDATE_DATA_HEADERS,
              data: {
                'content-type': 'application/json',
                [`x-hasura-${globals.adminSecretName}`]: adminSecret,
              },
            }),
            dispatch(
              changeRequestHeader(
                1,
                'key',
                `x-hasura-${globals.adminSecretName}`,
                true
              )
            ),
            dispatch(changeRequestHeader(1, 'value', adminSecret, true)),
          ]);
        })
        .then(() => {
          if (nextState.location.pathname === '/login') {
            replaceState('/');
          }
          cb();
        })
        .catch(() => {
          // Clear state from the localStorage if there exists one
          clearAdminSecretState();
          if (nextState.location.pathname !== '/login') {
            replaceState('/login');
          }
          cb();
        });
    } else {
      cb();
      return;
    }
  };
};

export default validateLogin;
