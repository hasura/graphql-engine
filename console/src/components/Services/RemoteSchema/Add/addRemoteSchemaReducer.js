import { addState } from '../state';
import { generateHeaderSyms } from '../../../Common/Layout/ReusableHeader/HeaderReducer';
import { makeRequest } from '../Actions';
import { appPrefix } from '../constants';
import { clearIntrospectionSchemaCache } from '../graphqlUtils';
import { exportMetadata } from '../../../../metadata/actions';
import { getRemoteSchemaSelector } from '../../../../metadata/selector';
import Migration from '../../../../utils/migration/Migration';
import { showErrorNotification } from '../../Common/Notification';
import {
  addRemoteSchemaQuery,
  removeRemoteSchemaQuery,
  updateRemoteSchemaQuery,
} from '../../../../metadata/queryUtils';
import _push from '../../Data/push';

const prefixUrl = appPrefix;

const MANUAL_URL_CHANGED = '@addRemoteSchema/MANUAL_URL_CHANGED';
const ENV_URL_CHANGED = '@addRemoteSchema/ENV_URL_CHANGED';
const NAME_CHANGED = '@addRemoteSchema/NAME_CHANGED';
const TIMEOUT_CONF_CHANGED = '@addRemoteSchema/TIMEOUT_CONF_CHANGED';
const COMMENT_CHANGED = '@addRemoteSchema/COMMENT_CHANGED';
// const HEADER_CHANGED = '@addRemoteSchema/HEADER_CHANGED';
const ADDING_REMOTE_SCHEMA = '@addRemoteSchema/ADDING_REMOTE_SCHEMA';
const ADD_REMOTE_SCHEMA_FAIL = '@addRemoteSchema/ADD_REMOTE_SCHEMA_FAIL';
const RESET = '@addRemoteSchema/RESET';
const FETCHING_INDIV_REMOTE_SCHEMA =
  '@addRemoteSchema/FETCHING_INDIV_REMOTE_SCHEMA';
const REMOTE_SCHEMA_FETCH_SUCCESS =
  '@addRemoteSchema/REMOTE_SCHEMA_FETCH_SUCCESS';
const REMOTE_SCHEMA_FETCH_FAIL = '@addRemoteSchema/REMOTE_SCHEMA_FETCH_FAIL';

const DELETING_REMOTE_SCHEMA = '@addRemoteSchema/DELETING_REMOTE_SCHEMA';
const DELETE_REMOTE_SCHEMA_FAIL = '@addRemoteSchema/DELETE_REMOTE_SCHEMA_FAIL';

const MODIFY_REMOTE_SCHEMA_FAIL = '@addRemoteSchema/MODIFY_REMOTE_SCHEMA_FAIL';
const MODIFYING_REMOTE_SCHEMA = '@addRemoteSchema/MODIFYING_REMOTE_SCHEMA';

const UPDATE_FORWARD_CLIENT_HEADERS =
  '@addRemoteSchema/UPDATE_FORWARD_CLIENT_HEADERS';

const TOGGLE_MODIFY = '@editRemoteSchema/TOGGLE_MODIFY';

const inputEventMap = {
  name: NAME_CHANGED,
  envName: ENV_URL_CHANGED,
  manualUrl: MANUAL_URL_CHANGED,
  timeoutConf: TIMEOUT_CONF_CHANGED,
  comment: COMMENT_CHANGED,
};

/* Action creators */
const inputChange = (type, data) => {
  return dispatch => dispatch({ type: inputEventMap[type], data });
};

const getHeaderEvents = generateHeaderSyms('REMOTE_SCHEMA');
/* */

const getReqHeader = headers => {
  const requestHeaders = [];

  const headersObj = headers.filter(h => h.name && h.name.length > 0);
  if (headersObj.length > 0) {
    headersObj.forEach(h => {
      const reqHead = {
        name: h.name,
      };

      if (h.type === 'static') {
        reqHead.value = h.value?.trim();
      } else {
        reqHead.value_from_env = h.value?.trim();
      }

      requestHeaders.push(reqHead);
    });
  }

  return requestHeaders;
};

const fetchRemoteSchema = remoteSchema => {
  return (dispatch, getState) => {
    const schema = getRemoteSchemaSelector(getState())(remoteSchema);
    if (schema) {
      dispatch({ type: REMOTE_SCHEMA_FETCH_SUCCESS, data: schema });
      const headerObj = [];
      (schema.definition.headers || []).forEach(d => {
        headerObj.push({
          name: d.name,
          value: d.value ? d.value : d.value_from_env,
          type: d.value ? 'static' : 'env',
        });
      });
      headerObj.push({
        name: '',
        type: 'static',
        value: '',
      });
      dispatch({
        type: getHeaderEvents.UPDATE_HEADERS,
        data: headerObj,
      });
    } else {
      dispatch(_push(`${prefixUrl}`));
    }
  };
};

const addRemoteSchema = () => (dispatch, getState) => {
  const currState = getState().remoteSchemas.addData;

  let timeoutSeconds = parseInt(currState.timeoutConf, 10);
  if (isNaN(timeoutSeconds)) timeoutSeconds = 60;

  const manualUrl = currState?.manualUrl?.trim();
  const envName = currState?.envName?.trim();
  const remoteSchemaName = currState.name.trim().replace(/ +/g, '');
  const remoteSchemaDef = {
    timeout_seconds: timeoutSeconds,
    forward_client_headers: currState.forwardClientHeaders,
    headers: getReqHeader(getState().remoteSchemas.headerData.headers),
  };
  const remoteSchemaComment = currState?.comment;

  if (!manualUrl && !envName) {
    dispatch(
      showErrorNotification(
        'Error in adding remote schema...',
        'A valid GraphQL server URL is required'
      )
    );
    return;
  } else if (manualUrl) {
    remoteSchemaDef.url = manualUrl;
  } else if (envName) {
    remoteSchemaDef.url_from_env = envName;
  }

  const migrationName = `create_remote_schema_${remoteSchemaName}`;
  const payload = addRemoteSchemaQuery(
    remoteSchemaName,
    remoteSchemaDef,
    remoteSchemaComment
  );
  const downPayload = removeRemoteSchemaQuery(remoteSchemaName);

  const requestMsg = 'Adding remote schema...';
  const successMsg = 'Remote schema added successfully';
  const errorMsg = 'Adding remote schema failed';

  const customOnSuccess = data => {
    Promise.all([
      dispatch({ type: RESET }),
      dispatch(exportMetadata()).then(() => {
        dispatch(_push(`${prefixUrl}/manage/${remoteSchemaName}/details`));
      }),
      dispatch({ type: getHeaderEvents.RESET_HEADER, data: data }),
    ]);
  };
  const customOnError = err => {
    console.error(`Failed to create remote schema ${JSON.stringify(err)}`);
    dispatch({ type: ADD_REMOTE_SCHEMA_FAIL, data: err });
  };
  dispatch({ type: ADDING_REMOTE_SCHEMA });

  return dispatch(
    makeRequest(
      [payload],
      [downPayload],
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    )
  );
};

const deleteRemoteSchema = () => (dispatch, getState) => {
  const currState = getState().remoteSchemas.addData;

  const remoteSchemaName = currState.editState.originalName;
  const remoteSchemaDef = {
    headers: currState.editState.originalHeaders,
    forward_client_headers: currState.editState.originalForwardClientHeaders,
    timeout_seconds: currState.editState.originalTimeoutConf,
  };
  const remoteSchemaComment = currState.editState?.originalComment ?? '';

  if (!currState.editState.originalUrl) {
    remoteSchemaDef.url_from_env = currState.editState.originalEnvUrl;
  } else if (!currState.editState.originalEnvUrl) {
    remoteSchemaDef.url = currState.editState.originalUrl;
  }

  const migrationName = `remove_remote_schema_${remoteSchemaName
    .trim()
    .replace(/ +/g, '')}`;
  const payload = removeRemoteSchemaQuery(remoteSchemaName);
  const downPayload = addRemoteSchemaQuery(
    remoteSchemaName,
    remoteSchemaDef,
    remoteSchemaComment
  );

  const requestMsg = 'Deleting remote schema...';
  const successMsg = 'Remote schema deleted successfully';
  const errorMsg = 'Delete remote schema failed';

  const customOnSuccess = () => {
    Promise.all([
      dispatch({ type: RESET }),
      dispatch(_push(prefixUrl)),
      dispatch(exportMetadata()),
    ]);
    clearIntrospectionSchemaCache();
  };
  const customOnError = error => {
    Promise.all([dispatch({ type: DELETE_REMOTE_SCHEMA_FAIL, data: error })]);
  };
  dispatch({ type: DELETING_REMOTE_SCHEMA });

  return dispatch(
    makeRequest(
      [payload],
      [downPayload],
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    )
  );
};

const modifyRemoteSchema = () => (dispatch, getState) => {
  const currState = getState().remoteSchemas.addData;

  let timeoutSeconds = parseInt(currState.timeoutConf, 10);
  if (isNaN(timeoutSeconds)) timeoutSeconds = 60;

  const manualUrl = currState?.manualUrl?.trim();
  const envName = currState?.envName?.trim();
  const remoteSchemaName = currState.name.trim().replace(/ +/g, '');
  const remoteSchemaDef = {
    timeout_seconds: timeoutSeconds,
    forward_client_headers: currState.forwardClientHeaders,
    headers: getReqHeader(getState().remoteSchemas.headerData.headers),
  };
  const remoteSchemaComment = currState?.comment;

  if (!manualUrl && !envName) {
    dispatch(
      showErrorNotification(
        'Error in adding remote schema...',
        'A valid GraphQL server URL is required'
      )
    );
    return;
  } else if (manualUrl) {
    remoteSchemaDef.url = manualUrl;
  } else if (envName) {
    remoteSchemaDef.url_from_env = envName;
  }

  const upQuery = updateRemoteSchemaQuery(
    remoteSchemaName,
    remoteSchemaDef,
    remoteSchemaComment
  );

  let oldTimeout = parseInt(currState?.editState?.originalTimeoutConf, 10);
  if (isNaN(oldTimeout)) oldTimeout = 60;

  const oldRemoteSchemaDef = {
    timeout_seconds: oldTimeout,
    headers: currState.editState.originalHeaders,
    forward_client_headers: currState.editState.originalForwardClientHeaders,
  };

  if (!currState.editState.originalUrl) {
    oldRemoteSchemaDef.url_from_env = currState.editState.originalEnvUrl;
  } else if (!currState.editState.originalEnvUrl) {
    oldRemoteSchemaDef.url = currState.editState.originalUrl;
  }

  const downQuery = updateRemoteSchemaQuery(
    remoteSchemaName,
    oldRemoteSchemaDef,
    currState.editState.originalComment
  );

  const migration = new Migration();
  const migrationName = `update_remote_schema_${remoteSchemaName}`;
  migration.add(upQuery, downQuery);

  const requestMsg = 'Modifying remote schema...';
  const successMsg = 'Remote schema modified';
  const errorMsg = 'Modify remote schema failed';

  const customOnSuccess = data => {
    dispatch({ type: RESET, data: data });
    dispatch(_push(`${prefixUrl}/manage/schemas`)); // to avoid 404
    dispatch(exportMetadata()).then(() => {
      dispatch(_push(`${prefixUrl}/manage/${remoteSchemaName}/details`));
      dispatch(fetchRemoteSchema(remoteSchemaName));
    });
    clearIntrospectionSchemaCache();
  };
  const customOnError = error => {
    Promise.all([dispatch({ type: MODIFY_REMOTE_SCHEMA_FAIL, data: error })]);
  };

  dispatch({ type: MODIFYING_REMOTE_SCHEMA });
  return dispatch(
    makeRequest(
      migration.upMigration,
      migration.downMigration,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    )
  );
};

const addRemoteSchemaReducer = (state = addState, action) => {
  switch (action.type) {
    case MANUAL_URL_CHANGED:
      return {
        ...state,
        manualUrl: action.data,
        envName: null,
      };
    case NAME_CHANGED:
      return {
        ...state,
        name: action.data,
      };
    case ENV_URL_CHANGED:
      return {
        ...state,
        envName: action.data,
        manualUrl: null,
      };
    case TIMEOUT_CONF_CHANGED:
      return {
        ...state,
        timeoutConf: action.data,
      };
    case COMMENT_CHANGED:
      return {
        ...state,
        comment: action.data,
      };
    case ADDING_REMOTE_SCHEMA:
      return {
        ...state,
        isRequesting: true,
        isError: null,
      };
    case ADD_REMOTE_SCHEMA_FAIL:
      return {
        ...state,
        isRequesting: false,
        isError: action.data,
      };
    case TOGGLE_MODIFY:
      return {
        ...state,
        headers: [...state.editState.headers],
        editState: {
          ...state.editState,
          isModify: !state.editState.isModify,
        },
      };

    case RESET:
      return {
        ...addState,
      };
    case FETCHING_INDIV_REMOTE_SCHEMA:
      return {
        ...state,
        isFetching: true,
        isFetchError: null,
      };
    case REMOTE_SCHEMA_FETCH_SUCCESS:
      return {
        ...state,
        name: action.data.name,
        manualUrl: action.data.definition.url || null,
        envName: action.data.definition.url_from_env || null,
        headers: action.data.definition.headers || [],
        timeoutConf: action.data.definition.timeout_seconds
          ? action.data.definition.timeout_seconds.toString()
          : '60',
        forwardClientHeaders: action.data.definition.forward_client_headers,
        comment: action.data?.comment || '',
        editState: {
          ...state,
          isModify: false,
          originalName: action.data.name,
          originalHeaders: action.data.definition.headers || [],
          originalUrl: action.data.definition.url || null,
          originalEnvUrl: action.data.definition.url_from_env || null,
          originalForwardClientHeaders:
            action.data.definition.forward_client_headers || false,
          originalComment: action.data?.comment || '',
        },
        isFetching: false,
        isFetchError: null,
      };
    case REMOTE_SCHEMA_FETCH_FAIL:
      return {
        ...state,
        isFetching: false,
        isFetchError: action.data,
      };
    case DELETE_REMOTE_SCHEMA_FAIL:
      return {
        ...state,
        isRequesting: false,
        isError: action.data,
      };
    case DELETING_REMOTE_SCHEMA:
      return {
        ...state,
        isRequesting: true,
        isError: null,
      };
    case MODIFY_REMOTE_SCHEMA_FAIL:
      return {
        ...state,
        isRequesting: false,
        isError: action.data,
      };
    case MODIFYING_REMOTE_SCHEMA:
      return {
        ...state,
        isRequesting: true,
        isError: null,
      };
    case UPDATE_FORWARD_CLIENT_HEADERS:
      return {
        ...state,
        forwardClientHeaders: !state.forwardClientHeaders,
      };
    default:
      return {
        ...state,
      };
  }
};

export {
  inputChange,
  addRemoteSchema,
  fetchRemoteSchema,
  deleteRemoteSchema,
  modifyRemoteSchema,
  RESET,
  TOGGLE_MODIFY,
  UPDATE_FORWARD_CLIENT_HEADERS,
  getHeaderEvents,
};

export default addRemoteSchemaReducer;
