import defaultState from './State';
import globals from '../../Globals';
import requestAction from '../../utils/requestAction';
import requestActionPlain from '../../utils/requestActionPlain';
import Endpoints, { globalCookiePolicy } from '../../Endpoints';
import { getFeaturesCompatibility } from '../../helpers/versionUtils';
import { getRunSqlQuery } from '../Common/utils/v1QueryUtils';
import { currentDriver } from '../../dataSources';
import { defaultNotification, errorNotification } from './ConsoleNotification';
import { updateConsoleNotificationsState } from '../../telemetry/Actions';
import { getConsoleNotificationQuery } from '../Common/utils/v1QueryUtils';
import dataHeaders from '../Services/Data/Common/Headers';
import { HASURA_COLLABORATOR_TOKEN } from '../../constants';
import { getUserType, getConsoleScope } from './utils';

const SET_MIGRATION_STATUS_SUCCESS = 'Main/SET_MIGRATION_STATUS_SUCCESS';
const SET_MIGRATION_STATUS_ERROR = 'Main/SET_MIGRATION_STATUS_ERROR';
const SET_SERVER_VERSION_SUCCESS = 'Main/SET_SERVER_VERSION_SUCCESS';
const SET_SERVER_VERSION_ERROR = 'Main/SET_SERVER_VERSION_ERROR';
const SET_LATEST_SERVER_VERSION_SUCCESS =
  'Main/SET_LATEST_SERVER_VERSION_SUCCESS';
const SET_LATEST_SERVER_VERSION_ERROR = 'Main/SET_LATEST_SERVER_VERSION_ERROR';
const UPDATE_MIGRATION_STATUS_SUCCESS = 'Main/UPDATE_MIGRATION_STATUS_SUCCESS';
const UPDATE_MIGRATION_STATUS_ERROR = 'Main/UPDATE_MIGRATION_STATUS_ERROR';
const HASURACTL_URL_ENV = 'Main/HASURACTL_URL_ENV';
const UPDATE_MIGRATION_MODE = 'Main/UPDATE_MIGRATION_MODE';
const UPDATE_MIGRATION_MODE_PROGRESS = 'Main/UPDATE_MIGRATION_MODE_PROGRESS';
const EXPORT_METADATA_SUCCESS = 'Main/EXPORT_METADATA_SUCCESS';
const EXPORT_METADATA_ERROR = 'Main/EXPORT_METADATA_ERROR';
const UPDATE_ADMIN_SECRET_INPUT = 'Main/UPDATE_ADMIN_SECRET_INPUT';
const LOGIN_IN_PROGRESS = 'Main/LOGIN_IN_PROGRESS';
const LOGIN_ERROR = 'Main/LOGIN_ERROR';
const POSTGRES_VERSION_SUCCESS = 'Main/POSTGRES_VERSION_SUCCESS';
const POSTGRES_VERSION_ERROR = 'Main/POSTGRES_VERSION_ERROR';
const FETCH_CONSOLE_NOTIFICATIONS_SUCCESS =
  'Main/FETCH_CONSOLE_NOTIFICATIONS_SUCCESS';
const FETCH_CONSOLE_NOTIFICATIONS_SET_DEFAULT =
  'Main/FETCH_CONSOLE_NOTIFICATIONS_SET_DEFAULT';
const FETCH_CONSOLE_NOTIFICATIONS_ERROR =
  'Main/FETCH_CONSOLE_NOTIFICATIONS_ERROR';
const FETCHING_HEROKU_SESSION = 'Main/FETCHING_HEROKU_SESSION';
const FETCHING_HEROKU_SESSION_FAILED = 'Main/FETCHING_HEROKU_SESSION_FAILED';
const SET_HEROKU_SESSION = 'Main/SET_HEROKU_SESSION';
const SET_CLOUD_PROJECT_INFO = 'Main/SET_CLOUD_PROJECT_INFO';

const RUN_TIME_ERROR = 'Main/RUN_TIME_ERROR';
const registerRunTimeError = data => ({
  type: RUN_TIME_ERROR,
  data,
});

/* Server config constants*/
const FETCHING_SERVER_CONFIG = 'Main/FETCHING_SERVER_CONFIG';
const SERVER_CONFIG_FETCH_SUCCESS = 'Main/SERVER_CONFIG_FETCH_SUCCESS';
const SERVER_CONFIG_FETCH_FAIL = 'Main/SERVER_CONFIG_FETCH_FAIL';
/* End */

// action definitions

const filterScope = (data, consoleScope) => {
  return data.filter(notif => {
    if (notif.scope.indexOf(consoleScope) !== -1) {
      return notif;
    }
  });
};

const makeUppercaseScopes = data => {
  return data.map(notif => {
    return { ...notif, scope: notif.scope.toUpperCase() };
  });
};

// to fetch and filter notifications
const fetchConsoleNotifications = () => (dispatch, getState) => {
  const url = !globals.isProduction
    ? Endpoints.consoleNotificationsStg
    : Endpoints.consoleNotificationsProd;
  const consoleStateDB = getState().telemetry.console_opts;
  let toShowBadge = true;
  const headers = dataHeaders(getState);
  let previousRead = null;
  const { serverVersion } = getState().main;
  const consoleId = window.__env.consoleId;
  const consoleScope = getConsoleScope(serverVersion, consoleId);
  let userType = 'admin';
  const headerHasCollabToken = Object.keys(headers).find(
    header => header.toLowerCase() === HASURA_COLLABORATOR_TOKEN
  );

  if (headerHasCollabToken) {
    const collabToken = headers[headerHasCollabToken];
    userType = getUserType(collabToken);
  }

  if (
    consoleStateDB &&
    consoleStateDB.console_notifications &&
    consoleStateDB.console_notifications[userType].date
  ) {
    toShowBadge = consoleStateDB.console_notifications[userType].showBadge;
    previousRead = consoleStateDB.console_notifications[userType].read;
  }

  const now = new Date().toISOString();
  const payload = getConsoleNotificationQuery(now, consoleScope);
  const options = {
    body: JSON.stringify(payload),
    method: 'POST',
    headers: {
      'content-type': 'application/json',
      // temp. change until Auth is added
      'x-hasura-role': 'user',
    },
  };

  return dispatch(requestAction(url, options))
    .then(data => {
      const lastSeenNotifications = JSON.parse(
        window.localStorage.getItem('notifications:lastSeen')
      );
      if (data.data.console_notifications) {
        const fetchedData = data.data.console_notifications;

        if (!fetchedData.length) {
          dispatch({ type: FETCH_CONSOLE_NOTIFICATIONS_SET_DEFAULT });
          dispatch(
            updateConsoleNotificationsState({
              read: 'default',
              date: now,
              showBadge: false,
            })
          );
          if (!lastSeenNotifications) {
            window.localStorage.setItem(
              'notifications:lastSeen',
              JSON.stringify(0)
            );
          }
          return;
        }

        // NOTE: these 2 steps may not be required if the table in the DB
        // enforces the usage of `enums` and we're sure that the notification scope
        // is only from the allowed permutations of scope. We aren't doing that yet
        // because within the GQL query, I can't be using the `_ilike` operator during
        // filtering. Hence I'm keeping it here since this is a new feature and
        // mistakes can happen while adding data into the DB.

        // TODO: is to remove these once things are more streamlined
        const uppercaseScopedData = makeUppercaseScopes(fetchedData);
        let filteredData = filterScope(uppercaseScopedData, consoleScope);

        if (
          lastSeenNotifications &&
          lastSeenNotifications > filteredData.length
        ) {
          window.localStorage.setItem(
            'notifications:lastSeen',
            JSON.stringify(filteredData.length)
          );
        }

        if (previousRead) {
          if (!consoleStateDB.console_notifications) {
            dispatch(
              updateConsoleNotificationsState({
                read: [],
                date: now,
                showBadge: true,
              })
            );
          } else {
            let newReadValue;
            if (previousRead === 'default' || previousRead === 'error') {
              newReadValue = [];
              toShowBadge = false;
            } else if (previousRead === 'all') {
              const previousList = JSON.parse(
                localStorage.getItem('notifications:data')
              );
              if (!previousList) {
                // we don't have a record of the IDs that were marked as read previously
                newReadValue = [];
                toShowBadge = true;
              } else if (previousList.length) {
                const readNotificationsDiff = filteredData.filter(
                  newNotif =>
                    !previousList.find(oldNotif => oldNotif.id === newNotif.id)
                );
                if (!readNotificationsDiff.length) {
                  // since the data hasn't changed since the last call
                  newReadValue = previousRead;
                  toShowBadge = false;
                } else {
                  newReadValue = [...previousList.map(notif => `${notif.id}`)];
                  toShowBadge = true;
                  filteredData = [...readNotificationsDiff, ...previousList];
                }
              }
            } else {
              newReadValue = previousRead;
              if (
                previousRead.length &&
                lastSeenNotifications >= filteredData.length
              ) {
                toShowBadge = false;
              } else if (lastSeenNotifications < filteredData.length) {
                toShowBadge = true;
              }
            }
            dispatch(
              updateConsoleNotificationsState({
                read: newReadValue,
                date: consoleStateDB.console_notifications[userType].date,
                showBadge: toShowBadge,
              })
            );
          }
        }

        dispatch({
          type: FETCH_CONSOLE_NOTIFICATIONS_SUCCESS,
          data: filteredData,
        });

        // update/set the lastSeen value upon data is set
        if (
          !lastSeenNotifications ||
          lastSeenNotifications !== filteredData.length
        ) {
          window.localStorage.setItem(
            'notifications:lastSeen',
            JSON.stringify(filteredData.length)
          );
        }
        return;
      }
      dispatch({ type: FETCH_CONSOLE_NOTIFICATIONS_ERROR });
      dispatch(
        updateConsoleNotificationsState({
          read: 'error',
          date: now,
          showBadge: false,
        })
      );
    })
    .catch(err => {
      console.error(err);
      dispatch({ type: FETCH_CONSOLE_NOTIFICATIONS_ERROR });
      dispatch(
        updateConsoleNotificationsState({
          read: 'error',
          date: now,
          showBadge: false,
        })
      );
    });
};

const SET_FEATURES_COMPATIBILITY = 'Main/SET_FEATURES_COMPATIBILITY';
const setFeaturesCompatibility = data => ({
  type: SET_FEATURES_COMPATIBILITY,
  data,
});

const PRO_CLICKED = 'Main/PRO_CLICKED';
const emitProClickedEvent = data => ({
  type: PRO_CLICKED,
  data,
});

const SET_READ_ONLY_MODE = 'Main/SET_READ_ONLY_MODE';
const setReadOnlyMode = data => ({
  type: SET_READ_ONLY_MODE,
  data,
});

export const fetchPostgresVersion = (dispatch, getState) => {
  if (currentDriver !== 'postgres') return;

  const req = getRunSqlQuery(
    'SELECT version()',
    getState().tables.currentDataSource,
    false,
    true
  );
  const options = {
    method: 'POST',
    credentials: globalCookiePolicy,
    body: JSON.stringify(req),
    headers: getState().tables.dataHeaders,
  };

  return dispatch(requestAction(Endpoints.query, options)).then(
    ({ result }) => {
      if (result.length > 1 && result[1].length) {
        const matchRes = result[1][0].match(/[0-9]{1,}(\.[0-9]{1,})?/);
        if (matchRes.length) {
          dispatch({ type: POSTGRES_VERSION_SUCCESS, payload: matchRes[0] });
          return;
        }
      }
      dispatch({ type: POSTGRES_VERSION_ERROR });
    }
  );
};

const featureCompatibilityInit = () => {
  return (dispatch, getState) => {
    const { serverVersion } = getState().main;

    if (!serverVersion) {
      return;
    }

    const featuresCompatibility = getFeaturesCompatibility(serverVersion);

    return dispatch(setFeaturesCompatibility(featuresCompatibility));
  };
};

const loadMigrationStatus = () => dispatch => {
  const url = Endpoints.hasuractlMigrateSettings;
  const options = {
    method: 'GET',
    credentials: globalCookiePolicy,
    headers: { 'content-type': 'application/json' },
  };
  return dispatch(
    requestAction(
      url,
      options,
      SET_MIGRATION_STATUS_SUCCESS,
      SET_MIGRATION_STATUS_ERROR
    )
  );
};

const loadServerVersion = () => dispatch => {
  const url = Endpoints.version;
  const options = {
    method: 'GET',
    credentials: globalCookiePolicy,
    headers: { 'content-type': 'application/json' },
  };
  return dispatch(requestActionPlain(url, options)).then(
    data => {
      let parsedVersion;
      try {
        parsedVersion = JSON.parse(data);
        dispatch({
          type: SET_SERVER_VERSION_SUCCESS,
          data: parsedVersion.version,
        });
      } catch (e) {
        console.error(e);
      }
    },
    error => {
      console.error(error);
      dispatch({ type: SET_SERVER_VERSION_ERROR, data: null });
    }
  );
};

const fetchServerConfig = (dispatch, getState) => {
  const url = Endpoints.serverConfig;
  const options = {
    method: 'GET',
    credentials: globalCookiePolicy,
    headers: getState().tables.dataHeaders,
  };
  dispatch({
    type: FETCHING_SERVER_CONFIG,
  });
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({
        type: SERVER_CONFIG_FETCH_SUCCESS,
        data: data,
      });
      globals.serverConfig = data;
      return Promise.resolve();
    },
    error => {
      dispatch({
        type: SERVER_CONFIG_FETCH_FAIL,
        data: error,
      });
      return Promise.reject();
    }
  );
};

const loadLatestServerVersion = () => (dispatch, getState) => {
  const url =
    Endpoints.updateCheck +
    '?agent=console&version=' +
    getState().main.serverVersion;
  const options = {
    method: 'GET',
    credentials: globalCookiePolicy,
    headers: { 'content-type': 'application/json' },
  };
  return dispatch(requestActionPlain(url, options)).then(
    data => {
      try {
        dispatch({
          type: SET_LATEST_SERVER_VERSION_SUCCESS,
          data: JSON.parse(data),
        });
      } catch (e) {
        console.error(e);
      }
    },
    error => {
      console.error(error);
      dispatch({ type: SET_LATEST_SERVER_VERSION_ERROR, data: null });
    }
  );
};

const updateMigrationModeStatus = () => (dispatch, getState) => {
  // make req to hasura cli to update migration mode
  dispatch({ type: UPDATE_MIGRATION_MODE_PROGRESS, data: true });
  const url = Endpoints.hasuractlMigrateSettings;
  const putBody = {
    name: 'migration_mode',
    value: (!getState().main.migrationMode).toString(),
  };
  const options = {
    method: 'PUT',
    credentials: globalCookiePolicy,
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(putBody),
  };
  return dispatch(requestAction(url, options, UPDATE_MIGRATION_MODE)).then(
    () => {
      // check if migration mode is off and send metadata export
      dispatch({ type: UPDATE_MIGRATION_MODE_PROGRESS, data: false });
      if (!getState().main.migrationMode) {
        // if its off
        const metadataOptions = {
          method: 'GET',
          credentials: globalCookiePolicy,
          headers: { 'content-type': 'application/json' },
        };
        const metadataUrl = `${Endpoints.hasuractlMetadata}?export=true`;
        return dispatch(
          requestAction(
            metadataUrl,
            metadataOptions,
            EXPORT_METADATA_SUCCESS,
            EXPORT_METADATA_ERROR
          )
        );
      }
    }
  );
  // refresh console
};

export const setHerokuSession = session => ({
  type: SET_HEROKU_SESSION,
  data: session,
});

// TODO to be queried via Apollo client
export const fetchHerokuSession = () => dispatch => {
  if (globals.consoleType !== 'cloud') {
    return;
  }
  dispatch({
    type: FETCHING_HEROKU_SESSION,
  });
  return fetch(Endpoints.luxDataGraphql, {
    method: 'POST',
    credentials: 'include',
    headers: {
      'content-type': 'application/json',
    },
    body: JSON.stringify({
      query:
        'mutation { getHerokuSession { access_token refresh_token expires_in token_type } }',
    }),
  })
    .then(r => r.json())
    .then(response => {
      if (response.errors) {
        dispatch({ type: FETCHING_HEROKU_SESSION_FAILED });
        console.error('Failed fetching heroku session');
      } else {
        const session = response.data.getHerokuSession;
        if (!session.access_token) {
          dispatch({ type: FETCHING_HEROKU_SESSION_FAILED });
          console.error('Failed fetching heroku session');
        } else {
          dispatch(setHerokuSession(session));
        }
      }
    })
    .catch(e => {
      console.error('Failed fetching Heroku session');
      console.error(e);
    });
};

const mainReducer = (state = defaultState, action) => {
  switch (action.type) {
    case SET_MIGRATION_STATUS_SUCCESS:
      return {
        ...state,
        migrationMode: action.data.migration_mode === 'true',
      };
    case SET_MIGRATION_STATUS_ERROR:
      return {
        ...state,
        migrationMode: action.data.migration_mode === 'true',
      };
    case SET_SERVER_VERSION_SUCCESS:
      return {
        ...state,
        serverVersion: action.data,
      };
    case SET_SERVER_VERSION_ERROR:
      return {
        ...state,
        serverVersion: null,
      };
    case SET_LATEST_SERVER_VERSION_SUCCESS:
      return {
        ...state,
        latestStableServerVersion: action.data.latest,
        latestPreReleaseServerVersion: action.data.prerelease,
      };
    case SET_LATEST_SERVER_VERSION_ERROR:
      return {
        ...state,
        latestStableServerVersion: null,
      };
    case UPDATE_MIGRATION_STATUS_SUCCESS:
      return {
        ...state,
        migrationMode: action.data.migration_mode === 'true',
      };
    case EXPORT_METADATA_SUCCESS:
      return {
        ...state,
        ...state.metadataExport,
        error: false,
        info: null,
      };
    case EXPORT_METADATA_ERROR:
      return {
        ...state,
        ...state.metadataExport,
        error: true,
        info: action.data,
      };
    case UPDATE_MIGRATION_MODE_PROGRESS:
      return {
        ...state,
        migrationModeProgress: action.data,
      };
    case UPDATE_MIGRATION_STATUS_ERROR:
      return { ...state, migrationError: action.data };
    case SET_READ_ONLY_MODE:
      return {
        ...state,
        readOnlyMode: action.data,
        migrationMode: !action.data, // HACK
      };
    case HASURACTL_URL_ENV:
      return { ...state, hasuractlEnv: action.data };
    case UPDATE_MIGRATION_MODE:
      const currentMode = state.migrationMode;
      return { ...state, migrationMode: !currentMode };
    case UPDATE_ADMIN_SECRET_INPUT:
      return { ...state, adminSecretInput: action.data };
    case LOGIN_IN_PROGRESS:
      return { ...state, loginInProgress: action.data };
    case LOGIN_ERROR:
      return { ...state, loginError: action.data };
    case RUN_TIME_ERROR: // To trigger telemetry event
      return state;
    case FETCHING_SERVER_CONFIG:
      return {
        ...state,
        serverConfig: {
          ...defaultState.serverConfig,
          isFetching: true,
        },
      };
    case SERVER_CONFIG_FETCH_SUCCESS:
      return {
        ...state,
        serverConfig: {
          ...state.serverConfig,
          data: {
            ...action.data,
          },
          isFetching: false,
        },
      };
    case SERVER_CONFIG_FETCH_FAIL:
      return {
        ...state,
        serverConfig: {
          ...state.serverConfig,
          error: action.data,
          isFetching: false,
        },
      };
    case SET_FEATURES_COMPATIBILITY:
      return {
        ...state,
        featuresCompatibility: { ...action.data },
      };
    case POSTGRES_VERSION_SUCCESS:
      return {
        ...state,
        postgresVersion: action.payload,
      };
    case POSTGRES_VERSION_ERROR:
      return {
        ...state,
        postgresVersion: null,
      };
    case FETCH_CONSOLE_NOTIFICATIONS_SUCCESS:
      return {
        ...state,
        consoleNotifications: action.data,
      };
    case FETCH_CONSOLE_NOTIFICATIONS_SET_DEFAULT:
      return {
        ...state,
        consoleNotifications: [defaultNotification],
      };
    case FETCH_CONSOLE_NOTIFICATIONS_ERROR:
      return {
        ...state,
        consoleNotifications: [errorNotification],
      };
    case SET_HEROKU_SESSION:
      return {
        ...state,
        heroku: {
          session: action.data,
        },
      };
    case SET_CLOUD_PROJECT_INFO:
      return {
        ...state,
        cloud: {
          project: action.data,
        },
      };
    default:
      return state;
  }
};

export default mainReducer;
export {
  HASURACTL_URL_ENV,
  UPDATE_MIGRATION_STATUS_SUCCESS,
  UPDATE_MIGRATION_STATUS_ERROR,
  UPDATE_ADMIN_SECRET_INPUT,
  loadMigrationStatus,
  setReadOnlyMode,
  updateMigrationModeStatus,
  LOGIN_IN_PROGRESS,
  LOGIN_ERROR,
  emitProClickedEvent,
  loadServerVersion,
  fetchServerConfig,
  loadLatestServerVersion,
  featureCompatibilityInit,
  RUN_TIME_ERROR,
  registerRunTimeError,
  fetchConsoleNotifications,
};
