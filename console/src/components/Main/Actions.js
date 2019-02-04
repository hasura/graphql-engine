import { push } from 'react-router-redux';
import globals from 'Globals';
import defaultState from './State';
import Endpoints from '../../Endpoints';
import requestAction from '../../utils/requestAction';
import requestActionPlain from '../../utils/requestActionPlain';
import { globalCookiePolicy } from '../../Endpoints';
import { saveAccessKeyState } from '../AppState';
import {
  ACCESS_KEY_ERROR,
  UPDATE_DATA_HEADERS,
} from '../Services/Data/DataActions';
import { changeRequestHeader } from '../ApiExplorer/Actions';

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
const UPDATE_ACCESS_KEY_INPUT = 'Main/UPDATE_ACCESS_KEY_INPUT';
const LOGIN_IN_PROGRESS = 'Main/LOGIN_IN_PROGRESS';
const LOGIN_ERROR = 'Main/LOGIN_ERROR';

const loadMigrationStatus = () => dispatch => {
  const url = Endpoints.hasuractlMigrateSettings;
  const options = {
    method: 'GET',
    credentials: globalCookiePolicy,
    headers: { 'Content-Type': 'application/json' },
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
    headers: { 'Content-Type': 'application/json' },
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

const checkServerUpdates = () => (dispatch, getState) => {
  const url =
    Endpoints.updateCheck +
    '?agent=console&version=' +
    getState().main.serverVersion;
  const options = {
    method: 'GET',
    credentials: globalCookiePolicy,
    headers: { 'Content-Type': 'application/json' },
  };
  return dispatch(requestActionPlain(url, options)).then(
    data => {
      let parsedVersion;
      try {
        parsedVersion = JSON.parse(data);
        dispatch({
          type: SET_LATEST_SERVER_VERSION_SUCCESS,
          data: parsedVersion.latest,
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

const validateLogin = isInitialLoad => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const currentSchema = getState().tables.currentSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: getState().tables.dataHeaders,
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
  if (isInitialLoad) {
    return dispatch(requestAction(url, options));
  }
  return dispatch(requestAction(url, options)).then(
    () => {
      dispatch({ type: LOGIN_IN_PROGRESS, data: false });
      dispatch({ type: LOGIN_ERROR, data: false });
      dispatch(push(globals.urlPrefix));
    },
    error => {
      dispatch({ type: LOGIN_IN_PROGRESS, data: false });
      dispatch({ type: LOGIN_ERROR, data: true });
      console.error('Failed to validate access key ' + JSON.stringify(error));
      if (error.code !== 'access-denied') {
        alert(JSON.stringify(error));
      }
    }
  );
};

const loginClicked = () => (dispatch, getState) => {
  // set localstorage
  dispatch({ type: LOGIN_IN_PROGRESS, data: true });
  const accessKeyInput = getState().main.accessKeyInput;
  saveAccessKeyState(accessKeyInput);
  // redirect to / to test the accessKeyInput;
  const updatedDataHeaders = {
    'Content-Type': 'application/json',
    'X-Hasura-Access-Key': accessKeyInput,
  };
  Promise.all([
    dispatch({ type: ACCESS_KEY_ERROR, data: false }),
    dispatch({ type: UPDATE_DATA_HEADERS, data: updatedDataHeaders }),
    dispatch(changeRequestHeader(1, 'key', 'X-Hasura-Access-Key', true)),
    dispatch(changeRequestHeader(1, 'value', accessKeyInput, true)),
    // dispatch(push('/'))
  ]).then(() => {
    // make a sample query. check error code and push to /
    dispatch(validateLogin(false));
  });
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
    headers: { 'Content-Type': 'application/json' },
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
          headers: { 'Content-Type': 'application/json' },
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
        latestServerVersion: action.data,
      };
    case SET_LATEST_SERVER_VERSION_ERROR:
      return {
        ...state,
        latestServerVersion: null,
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
    case HASURACTL_URL_ENV:
      return { ...state, hasuractlEnv: action.data };
    case UPDATE_MIGRATION_MODE:
      const currentMode = state.migrationMode;
      return { ...state, migrationMode: !currentMode };
    case UPDATE_ACCESS_KEY_INPUT:
      return { ...state, accessKeyInput: action.data };
    case LOGIN_IN_PROGRESS:
      return { ...state, loginInProgress: action.data };
    case LOGIN_ERROR:
      return { ...state, loginError: action.data };

    default:
      return state;
  }
};

export default mainReducer;
export {
  HASURACTL_URL_ENV,
  UPDATE_MIGRATION_STATUS_SUCCESS,
  UPDATE_MIGRATION_STATUS_ERROR,
  UPDATE_ACCESS_KEY_INPUT,
  loadMigrationStatus,
  updateMigrationModeStatus,
  loginClicked,
  LOGIN_IN_PROGRESS,
  LOGIN_ERROR,
  validateLogin,
  loadServerVersion,
  checkServerUpdates,
};
