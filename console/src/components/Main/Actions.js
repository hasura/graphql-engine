import defaultState from './State';
import globals from '../../Globals';
import requestAction from '../../utils/requestAction';
import requestActionPlain from '../../utils/requestActionPlain';
import Endpoints, { globalCookiePolicy } from '../../Endpoints';
import fetch from 'isomorphic-fetch';
import { getFeaturesCompatibility } from '../../helpers/versionUtils';

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

const SERVER_HEALTH_STATUS = 'Main/SERVER_HEALTH_STATUS';

const checkServerHealth = () => {
  const url = Endpoints.healthCheck;
  const options = {
    method: "GET"
  };
  return fetch(url, options).then(response => {
    if (response.status === 200) {
      dispatch({ type: SERVER_HEALTH_STATUS, data: true });
      return;
    }

    if (response.status === 500) {
      dispatch({ type: SERVER_HEALTH_STATUS, data: false });
      return;
    }
  })
}

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

const fetchServerConfig = () => (dispatch, getState) => {
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
    case SERVER_HEALTH_STATUS: 
      return {
        ...state,
        serverHealth: { ...action.data },
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
  checkServerHealth
};
