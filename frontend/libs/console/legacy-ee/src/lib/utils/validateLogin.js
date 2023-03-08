import {
  changeRequestHeader,
  UPDATE_DATA_HEADERS,
} from '@hasura/console-legacy-ce';
import {
  clearAdminSecretState,
  clearPATState,
  loadAdminSecretState,
  loadPATState,
} from '../components/AppState';
import {
  getHeaders,
  SET_METADATA,
  UPDATE_PROJECT_ID,
  UPDATE_PROJECT_NAME,
} from '../components/Main/Actions';
import { OAUTH_CALLBACK_URL, SERVER_CONSOLE_MODE } from '../constants';
import Endpoints, { globalCookiePolicy } from '../Endpoints';
import extendedGlobals from '../Globals';
import globals from '../Globals';
import { parseQueryString } from '../helpers/parseQueryString';
import requestActionPlain from './requestActionPlain';
import { constructRedirectUrl, isJsonString } from './utils';

const checkValidity = () => {
  return dispatch => {
    const url = Endpoints.metadata;
    const options = {
      method: 'POST',
      credentials: globalCookiePolicy,
      headers: getHeaders('adminSecret'),
      body: JSON.stringify({ type: 'export_metadata', args: {} }),
    };
    return dispatch(requestActionPlain(url, options));
  };
};

const checkValidityPAT = () => {
  return dispatch => {
    const url = Endpoints.metadata;
    const options = {
      method: 'POST',
      credentials: globalCookiePolicy,
      headers: getHeaders('pat'),
      body: JSON.stringify({ type: 'export_metadata', args: {} }),
    };
    return dispatch(requestActionPlain(url, options)).then(
      data =>
        dispatch({
          type: SET_METADATA,
          data: { ...data, loading: false },
        }),
      console.error
    );
  };
};

const validateLogin = ({ dispatch }) => {
  return (nextState, replaceState, cb) => {
    if (nextState.location.pathname === OAUTH_CALLBACK_URL) {
      cb();
    }
    const { pathname, search } = nextState.location;
    const personalAccessToken = loadPATState();

    // Validate isAdminSecretSet env is set by server or adminSecret env is set by cli
    // New: Checking for adminSecret now preceds checking for PersonalAccessToken cos of Team console
    if (globals.isAdminSecretSet || globals.adminSecret) {
      let adminSecret = '';
      // Check the console mode and retrieve adminSecret accordingly.
      if (globals.consoleMode === SERVER_CONSOLE_MODE) {
        adminSecret =
          extendedGlobals.adminSecret ||
          loadAdminSecretState() ||
          globals.adminSecret;
      } else {
        adminSecret = globals.adminSecret;
      }
      dispatch(checkValidity(adminSecret))
        .then(data => {
          if (isJsonString(data)) {
            const metadata = JSON.parse(data);
            dispatch({
              type: SET_METADATA,
              data: { ...metadata, loading: false },
            });
          }
          return Promise.all([
            dispatch({
              type: UPDATE_DATA_HEADERS,
              data: getHeaders('adminSecret'),
            }),
            dispatch(
              changeRequestHeader(
                1,
                'key',
                `x-hasura-${globals.adminSecretLabel}`,
                true
              )
            ),
            dispatch(changeRequestHeader(1, 'value', adminSecret, true)),
          ]);
        })
        .then(() => {
          // Is this causing the error to route to the login page?
          if (nextState.location.pathname === '/login') {
            replaceState('/');
          }
          cb();
        })
        .catch(err => {
          console.error(err);
          // Clear state from the localStorage if there exists one
          clearAdminSecretState();
          if (nextState.location.pathname !== '/login') {
            // Check if a query param `auto_login=true` is set,
            // if it is set: pass it on
            const p = parseQueryString(search);
            const appendAutoLoginIfAvailable = () => {
              if (
                p &&
                'auto_login' in p &&
                (p.auto_login || p.auto_login === 'true')
              ) {
                return '&auto_login=true';
              }
              return '';
            };
            replaceState({
              pathname: '/login',
              search: `?redirect_url=${window.encodeURIComponent(
                constructRedirectUrl(pathname, search)
              )}${appendAutoLoginIfAvailable(p)}`,
            });
          }
          cb();
        });
    } else if (personalAccessToken) {
      // Check the console mode and retrieve adminSecret accordingly.
      dispatch(checkValidityPAT(personalAccessToken))
        .then(() => {
          return Promise.all([
            dispatch({
              type: UPDATE_DATA_HEADERS,
              data: getHeaders('pat'),
            }),
            dispatch(changeRequestHeader(1, 'key', globals.patLabel, true)),
            dispatch(
              changeRequestHeader(
                1,
                'value',
                `pat ${personalAccessToken}`,
                true
              )
            ),
          ]);
        })
        .then(() => {
          dispatch({
            type: UPDATE_PROJECT_ID,
            data: globals.projectId,
          });
          dispatch({
            type: UPDATE_PROJECT_NAME,
            data: globals.projectName,
          });
          if (nextState.location.pathname === '/login') {
            replaceState('/');
          }
          cb();
        })
        .catch(() => {
          // Clear state from the localStorage if there exists one
          clearPATState();
          if (nextState.location.pathname !== '/login') {
            replaceState({
              pathname: '/login',
              search: `?redirect_url=${window.encodeURIComponent(
                constructRedirectUrl(pathname, search)
              )}`,
            });
          }
          cb();
        });
    } else if (globals.pro === true) {
      if (nextState.location.pathname !== '/login') {
        replaceState({
          pathname: '/login',
          search: `?redirect_url=${window.encodeURIComponent(
            constructRedirectUrl(pathname, search)
          )}`,
        });
      }
      cb();
    } else {
      cb();
      return;
    }
  };
};

export default validateLogin;
