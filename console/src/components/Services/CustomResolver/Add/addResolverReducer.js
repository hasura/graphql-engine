/* defaultState */
import { addState } from '../state';
/* */

import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import requestAction from '../../../../utils/requestAction';
import dataHeaders from '../../Data/Common/Headers';
import { push } from 'react-router-redux';
import { fetchResolvers } from '../customActions';

import { generateHeaderSyms } from '../../../Common/Layout/ReusableHeader/HeaderReducer';
import { makeRequest } from '../customActions';
// import { UPDATE_MIGRATION_STATUS_ERROR } from '../../../Main/Actions';
import { appPrefix } from '../constants';

import globals from '../../../../Globals';

const prefixUrl = globals.urlPrefix + appPrefix;

/* */
const MANUAL_URL_CHANGED = '@addResolver/MANUAL_URL_CHANGED';
const ENV_URL_CHANGED = '@addResolver/ENV_URL_CHANGED';
const NAME_CHANGED = '@addResolver/NAME_CHANGED';
// const HEADER_CHANGED = '@addResolver/HEADER_CHANGED';
const ADDING_RESOLVER = '@addResolver/ADDING_RESOLVER';
const ADD_RESOLVER_FAIL = '@addResolver/ADD_RESOLVER_FAIL';
const RESET = '@addResolver/RESET';
const FETCHING_INDIV_RESOLVER = '@addResolver/FETCHING_INDIV_RESOLVER';
const RESOLVER_FETCH_SUCCESS = '@addResolver/RESOLVER_FETCH_SUCCESS';
const RESOLVER_FETCH_FAIL = '@addResolver/RESOLVER_FETCH_FAIL';

const DELETING_RESOLVER = '@addResolver/DELETING_RESOLVER';
const DELETE_RESOLVER_FAIL = '@addResolver/DELETE_RESOLVER_FAIL';

const MODIFY_RESOLVER_FAIL = '@addResolver/MODIFY_RESOLVER_FAIL';
const MODIFYING_RESOLVER = '@addResolver/MODIFYING_RESOLVER';

const UPDATE_FORWARD_CLIENT_HEADERS =
  '@addResolver/UPDATE_FORWARD_CLIENT_HEADERS';

/* */
const TOGGLE_MODIFY = '@editResolver/TOGGLE_MODIFY';
/* */
/* */

const inputEventMap = {
  name: NAME_CHANGED,
  envName: ENV_URL_CHANGED,
  manualUrl: MANUAL_URL_CHANGED,
};

/* Action creators */
const inputChange = (type, data) => {
  return dispatch => dispatch({ type: inputEventMap[type], data });
};

const getHeaderEvents = generateHeaderSyms('CUSTOM_RESOLVER');
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
        reqHead.value = h.value;
      } else {
        reqHead.value_from_env = h.value;
      }

      requestHeaders.push(reqHead);
    });
  }

  return requestHeaders;
};

const fetchResolver = resolver => {
  return (dispatch, getState) => {
    const url = Endpoints.getSchema;
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify({
        type: 'select',
        args: {
          table: {
            name: 'remote_schemas',
            schema: 'hdb_catalog',
          },
          columns: ['*'],
          where: {
            name: resolver,
          },
        },
      }),
    };
    dispatch({ type: FETCHING_INDIV_RESOLVER });
    return dispatch(requestAction(url, options)).then(
      data => {
        if (data.length > 0) {
          dispatch({ type: RESOLVER_FETCH_SUCCESS, data: data });
          const headerObj = [];
          data[0].definition.headers.forEach(d => {
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
            data: [...headerObj],
          });
          return Promise.resolve();
        }
        return dispatch(push(`${prefixUrl}`));
      },
      error => {
        console.error('Failed to fetch resolver' + JSON.stringify(error));
        return dispatch({ type: RESOLVER_FETCH_FAIL, data: error });
      }
    );
  };
};

const addResolver = () => {
  return (dispatch, getState) => {
    const currState = getState().customResolverData.addData;
    // const url = Endpoints.getSchema;
    const resolveObj = {
      name: currState.name.trim().replace(/ +/g, ''),
      definition: {
        url: currState.manualUrl,
        url_from_env: currState.envName,
        headers: [],
        forward_client_headers: currState.forwardClientHeaders,
      },
    };

    resolveObj.definition.headers = [
      ...getReqHeader(getState().customResolverData.headerData.headers),
    ];
    if (resolveObj.definition.url) {
      delete resolveObj.definition.url_from_env;
    } else {
      delete resolveObj.definition.url;
    }
    /* TODO: Add mandatory fields validation */

    const migrationName =
      'create_remote_schema_' + currState.name.trim().replace(/ +/g, '');

    const payload = {
      type: 'add_remote_schema',
      args: {
        ...resolveObj,
      },
    };

    const downPayload = {
      type: 'remove_remote_schema',
      args: {
        name: currState.name,
      },
    };

    const upQueryArgs = [];
    upQueryArgs.push(payload);
    const downQueryArgs = [];
    downQueryArgs.push(downPayload);
    const upQuery = {
      type: 'bulk',
      args: upQueryArgs,
    };

    const downQuery = {
      type: 'bulk',
      args: downQueryArgs,
    };

    const requestMsg = 'Adding remote schema...';
    const successMsg = 'Remote schema added successfully';
    const errorMsg = 'Adding remote schema failed';

    const customOnSuccess = data => {
      Promise.all([
        dispatch({ type: RESET }),
        dispatch(fetchResolvers()).then(() => {
          dispatch(push(`${prefixUrl}/manage/${resolveObj.name}/details`));
        }),
        dispatch({ type: getHeaderEvents.RESET_HEADER, data: data }),
      ]);
    };
    const customOnError = err => {
      console.error('Failed to create remote schema' + JSON.stringify(err));
      dispatch({ type: ADD_RESOLVER_FAIL, data: err });
      // dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
      // alert(JSON.stringify(err));
    };
    dispatch({ type: ADDING_RESOLVER });
    return dispatch(
      makeRequest(
        upQuery.args,
        downQuery.args,
        migrationName,
        customOnSuccess,
        customOnError,
        requestMsg,
        successMsg,
        errorMsg
      )
    );
  };
};

const deleteResolver = () => {
  return (dispatch, getState) => {
    const currState = getState().customResolverData.addData;
    // const url = Endpoints.getSchema;
    const resolveObj = {
      name: currState.editState.originalName,
    };
    const migrationName =
      'remove_remote_schema_' + resolveObj.name.trim().replace(/ +/g, '');
    const payload = {
      type: 'remove_remote_schema',
      args: {
        name: currState.editState.originalName,
      },
    };
    const downPayload = {
      type: 'add_remote_schema',
      args: {
        name: currState.editState.originalName,
        definition: {
          url: currState.editState.originalUrl,
          url_from_env: currState.editState.originalEnvUrl,
          headers: [],
          forward_client_headers:
            currState.editState.originalForwardClientHeaders,
        },
      },
    };

    downPayload.args.definition.headers = [
      ...currState.editState.originalHeaders,
    ];

    const upQueryArgs = [];
    upQueryArgs.push(payload);
    const downQueryArgs = [];
    downQueryArgs.push(downPayload);
    const upQuery = {
      type: 'bulk',
      args: upQueryArgs,
    };
    const downQuery = {
      type: 'bulk',
      args: downQueryArgs,
    };
    const requestMsg = 'Deleting remote schema...';
    const successMsg = 'Remote schema deleted successfully';
    const errorMsg = 'Delete remote schema failed';

    const customOnSuccess = () => {
      // dispatch({ type: REQUEST_SUCCESS });
      Promise.all([
        dispatch({ type: RESET }),
        dispatch(push(prefixUrl)),
        dispatch(fetchResolvers()),
      ]);
    };
    const customOnError = error => {
      Promise.all([dispatch({ type: DELETE_RESOLVER_FAIL, data: error })]);
    };

    dispatch({ type: DELETING_RESOLVER });
    return dispatch(
      makeRequest(
        upQuery.args,
        downQuery.args,
        migrationName,
        customOnSuccess,
        customOnError,
        requestMsg,
        successMsg,
        errorMsg
      )
    );
  };
};

const modifyResolver = () => {
  return (dispatch, getState) => {
    const currState = getState().customResolverData.addData;
    const remoteSchemaName = currState.name.trim().replace(/ +/g, '');
    // const url = Endpoints.getSchema;
    const upQueryArgs = [];
    const downQueryArgs = [];
    const migrationName = 'update_remote_schema_' + remoteSchemaName;
    const deleteResolverUp = {
      type: 'remove_remote_schema',
      args: {
        name: currState.editState.originalName,
      },
    };
    const resolveObj = {
      name: remoteSchemaName,
      definition: {
        url: currState.manualUrl,
        url_from_env: currState.envName,
        forward_client_headers: currState.forwardClientHeaders,
        headers: [],
      },
    };

    resolveObj.definition.headers = [
      ...getReqHeader(getState().customResolverData.headerData.headers),
    ];
    if (resolveObj.definition.url) {
      delete resolveObj.definition.url_from_env;
    } else {
      delete resolveObj.definition.url;
    }

    const createResolverUp = {
      type: 'add_remote_schema',
      args: {
        ...resolveObj,
      },
    };
    upQueryArgs.push(deleteResolverUp);
    upQueryArgs.push(createResolverUp);

    // Delete the new one and create the old one
    const deleteResolverDown = {
      type: 'remove_remote_schema',
      args: {
        name: remoteSchemaName,
      },
    };
    const resolveDownObj = {
      name: currState.editState.originalName,
      definition: {
        url: currState.editState.originalUrl,
        url_from_env: currState.editState.originalEnvUrl,
        headers: [],
        forward_client_headers:
          currState.editState.originalForwardClientHeaders,
      },
    };

    resolveDownObj.definition.headers = [
      ...currState.editState.originalHeaders,
    ];
    if (resolveDownObj.definition.url) {
      delete resolveDownObj.definition.url_from_env;
    } else {
      delete resolveDownObj.definition.url;
    }

    const createResolverDown = {
      type: 'add_remote_schema',
      args: {
        ...resolveDownObj,
      },
    };
    downQueryArgs.push(deleteResolverDown);
    downQueryArgs.push(createResolverDown);
    // End of down

    const upQuery = {
      type: 'bulk',
      args: upQueryArgs,
    };
    const downQuery = {
      type: 'bulk',
      args: downQueryArgs,
    };
    const requestMsg = 'Modifying remote schema...';
    const successMsg = 'Remote schema modified';
    const errorMsg = 'Modify remote schema failed';

    const customOnSuccess = data => {
      // dispatch({ type: REQUEST_SUCCESS });
      dispatch({ type: RESET, data: data });
      dispatch(push(`${prefixUrl}/manage/schemas`)); // to avoid 404
      dispatch(fetchResolvers()).then(() => {
        dispatch(push(`${prefixUrl}/manage/${remoteSchemaName}/details`));
      });
      dispatch(fetchResolver(remoteSchemaName));
    };
    const customOnError = error => {
      Promise.all([dispatch({ type: MODIFY_RESOLVER_FAIL, data: error })]);
    };

    dispatch({ type: MODIFYING_RESOLVER });
    return dispatch(
      makeRequest(
        upQuery.args,
        downQuery.args,
        migrationName,
        customOnSuccess,
        customOnError,
        requestMsg,
        successMsg,
        errorMsg
      )
    );
  };
};

const addResolverReducer = (state = addState, action) => {
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
    case ADDING_RESOLVER:
      return {
        ...state,
        isRequesting: true,
        isError: null,
      };
    case ADD_RESOLVER_FAIL:
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
    case FETCHING_INDIV_RESOLVER:
      return {
        ...state,
        isFetching: true,
        isFetchError: null,
      };
    case RESOLVER_FETCH_SUCCESS:
      return {
        ...state,
        name: action.data[0].name,
        manualUrl: action.data[0].definition.url || null,
        envName: action.data[0].definition.url_from_env || null,
        headers: action.data[0].definition.headers || [],
        forwardClientHeaders: action.data[0].definition.forward_client_headers,
        editState: {
          ...state,
          id: action.data[0].id,
          isModify: false,
          originalName: action.data[0].name,
          originalHeaders: action.data[0].definition.headers || [],
          originalUrl: action.data[0].definition.url || null,
          originalEnvUrl: action.data[0].definition.url_from_env || null,
          originalForwardClientHeaders:
            action.data[0].definition.forward_client_headers || false,
        },
        isFetching: false,
        isFetchError: null,
      };
    case RESOLVER_FETCH_FAIL:
      return {
        ...state,
        isFetching: false,
        isFetchError: action.data,
      };
    case DELETE_RESOLVER_FAIL:
      return {
        ...state,
        isRequesting: false,
        isError: action.data,
      };
    case DELETING_RESOLVER:
      return {
        ...state,
        isRequesting: true,
        isError: null,
      };
    case MODIFY_RESOLVER_FAIL:
      return {
        ...state,
        isRequesting: false,
        isError: action.data,
      };
    case MODIFYING_RESOLVER:
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
  addResolver,
  fetchResolver,
  deleteResolver,
  modifyResolver,
  RESET,
  TOGGLE_MODIFY,
  UPDATE_FORWARD_CLIENT_HEADERS,
  getHeaderEvents,
};

export default addResolverReducer;
