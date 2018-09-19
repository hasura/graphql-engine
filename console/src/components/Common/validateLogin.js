import { loadAccessKeyState, clearAccessKeyState } from '../AppState';
import globals from '../../Globals';
import Endpoints, { globalCookiePolicy } from '../../Endpoints';
import requestAction from '../../utils/requestAction';

import { UPDATE_DATA_HEADERS } from '../Services/Data/DataActions';
import { changeRequestHeader } from '../ApiExplorer/Actions';

const checkValidity = accessKey => {
  return dispatch => {
    const url = Endpoints.getSchema;
    const currentSchema = 'public';
    const headers = {
      'Content-Type': 'application/json',
      'X-Hasura-Access-Key': accessKey,
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
    // Validate isAccessKeySet env is set by hasuradb or accessKey env is set by cli
    if (globals.isAccessKeySet || globals.accessKey) {
      let accessKey = '';
      // Check the console mode and retrieve accessKey accordingly.
      if (globals.consoleMode === 'hasuradb') {
        accessKey = loadAccessKeyState('CONSOLE_ACCESS_KEY');
      } else {
        accessKey = globals.accessKey;
      }
      dispatch(checkValidity(accessKey))
        .then(() => {
          return Promise.all([
            dispatch({
              type: UPDATE_DATA_HEADERS,
              data: {
                'Content-Type': 'application/json',
                'X-Hasura-Access-Key': accessKey,
              },
            }),
            dispatch(
              changeRequestHeader(1, 'key', 'X-Hasura-Access-Key', true)
            ),
            dispatch(changeRequestHeader(1, 'value', accessKey, true)),
          ]);
        })
        .then(() => {
          if (nextState.location.pathname === '/login') {
            replaceState(globals.urlPrefix + '/');
          }
          cb();
        })
        .catch(() => {
          // Clear state from the localStorage if there exists one
          clearAccessKeyState();
          if (nextState.location.pathname !== '/login') {
            replaceState(globals.urlPrefix + '/login');
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
