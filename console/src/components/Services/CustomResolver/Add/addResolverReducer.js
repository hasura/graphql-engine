/* defaultState */
import { addState } from '../state';
/* */

import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import requestAction from '../../../../utils/requestAction';
import dataHeaders from '../../Data/Common/Headers';
import { push } from 'react-router-redux';
import { fetchResolvers } from '../customActions';

import { generateHeaderSyms } from '../../Layout/ReusableHeader/HeaderReducer';
import { makeRequest } from '../customActions';
// import { UPDATE_MIGRATION_STATUS_ERROR } from '../../../Main/Actions';

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
  console.log('type');
  return dispatch => dispatch({ type: inputEventMap[type], data });
};

const getHeaderEvents = generateHeaderSyms('CUSTOM_RESOLVER');
/* */

const getReqHeader = headers => {
  const headersObj = headers.filter(h => {
    return h.name && h.name.length > 0;
  });
  const requestHeader =
    headersObj.length > 0
      ? headersObj.map(h => {
        const reqHead = {
          name: h.name,
        };
        if (h.type === 'static') {
          reqHead.value = h.value;
        } else {
          reqHead.value_from_env = h.value;
        }
        return reqHead;
      })
      : [];
  return requestHeader;
};

const addResolver = () => {
  return (dispatch, getState) => {
    const currState = getState().customResolverData.addData;
    // const url = Endpoints.getSchema;
    const resolveObj = {
      name: currState.name,
      url: currState.manualUrl,
      url_from_env: currState.envName,
      headers: [],
    };

    resolveObj.headers = [
      ...getReqHeader(getState().customResolverData.headerData.headers),
    ];
    if (resolveObj.url) {
      delete resolveObj.url_from_env;
    } else {
      delete resolveObj.url;
    }
    /* TODO: Add mandatory fields validation */

    const migrationName = 'create_resolver_' + currState.name.trim();

    const payload = {
      type: 'add_custom_resolver',
      args: {
        ...resolveObj,
      },
    };

    const downPayload = {
      type: 'delete_custom_resolver',
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

    const requestMsg = 'Creating resolver ...';
    const successMsg = 'Resolver created';
    const errorMsg = 'Create resolver failed';

    const customOnSuccess = data => {
      Promise.all([
        dispatch({ type: RESET, data: data }),
        dispatch(push(`/custom-resolver/manage/${resolveObj.name}/edit`)),
        dispatch(fetchResolvers()),
        dispatch({ type: getHeaderEvents.RESET_HEADER, data: data }),
      ]);
    };
    const customOnError = err => {
      console.error('Failed to delete triggers' + JSON.stringify(err));
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
    /*
    return dispatch(requestAction(url, options)).then(
      data => {
        return Promise.all([
          dispatch({ type: RESET, data: data }),
          dispatch(push(`/custom-resolver/manage/${resolveObj.name}/edit`)),
          dispatch(fetchResolvers()),
          dispatch({ type: getHeaderEvents.RESET, data: data }),
          Promise.resolve(),
        ]);
      },
      error => {
        console.error('Failed to delete triggers' + JSON.stringify(error));
        dispatch({ type: ADD_RESOLVER_FAIL, data: error });
        alert(JSON.stringify(error));
        return Promise.reject();
      }
    );
    */
  };
};

const deleteResolver = () => {
  return (dispatch, getState) => {
    const currState = getState().customResolverData.addData;
    // const url = Endpoints.getSchema;
    const resolveObj = {
      name: currState.editState.originalName,
    };
    const migrationName = 'delete_resolver_' + resolveObj.name.trim();
    const payload = {
      type: 'delete_custom_resolver',
      args: {
        name: currState.editState.originalName,
      },
    };
    const downPayload = {
      type: 'add_custom_resolver',
      args: {
        name: currState.editState.originalName,
        url: currState.manualUrl,
        url_from_env: currState.envName,
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
    const requestMsg = 'Deleting resolver...';
    const successMsg = 'Resolver deleted';
    const errorMsg = 'Delete resolver failed';

    const customOnSuccess = data => {
      // dispatch({ type: REQUEST_SUCCESS });
      return Promise.all([
        dispatch({ type: RESET, data: data }),
        dispatch(push('/custom-resolver')),
        dispatch(fetchResolvers()),
      ]);
    };
    const customOnError = error => {
      return Promise.all([
        dispatch({ type: DELETE_RESOLVER_FAIL, data: error }),
      ]);
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
    // const url = Endpoints.getSchema;
    const upQueryArgs = [];
    const downQueryArgs = [];
    const migrationName = 'update_resolver_' + currState.name.trim();
    const deleteResolverUp = {
      type: 'delete_custom_resolver',
      args: {
        name: currState.editState.originalName,
      },
    };
    const resolveObj = {
      name: currState.name,
      url: currState.manualUrl,
      url_from_env: currState.envName,
      headers: [],
    };

    resolveObj.headers = [
      ...getReqHeader(getState().customResolverData.headerData.headers),
    ];
    if (resolveObj.url) {
      delete resolveObj.url_from_env;
    } else {
      delete resolveObj.url;
    }
    /* TODO: Add mandatory fields validation */

    const createResolverUp = {
      type: 'add_custom_resolver',
      args: {
        ...resolveObj,
      },
    };
    upQueryArgs.push(deleteResolverUp);
    upQueryArgs.push(createResolverUp);

    /* Down */
    // Delete the new one and create the old one
    const deleteResolverDown = {
      type: 'delete_custom_resolver',
      args: {
        name: currState.name,
      },
    };
    const resolveDownObj = {
      name: currState.editState.originalName,
      url: currState.editState.originalUrl,
      url_from_env: currState.editState.originalEnvUrl,
      headers: [],
    };

    resolveDownObj.headers = [...currState.editState.originalHeaders];
    if (resolveDownObj.url) {
      delete resolveDownObj.url_from_env;
    } else {
      delete resolveDownObj.url;
    }
    /* TODO: Add mandatory fields validation */

    const createResolverDown = {
      type: 'add_custom_resolver',
      args: {
        ...resolveDownObj,
      },
    };
    downQueryArgs.push(deleteResolverDown);
    downQueryArgs.push(createResolverDown);
    /* */

    const upQuery = {
      type: 'bulk',
      args: upQueryArgs,
    };
    const downQuery = {
      type: 'bulk',
      args: downQueryArgs,
    };
    const requestMsg = 'Modifying resolver...';
    const successMsg = 'Resolver modified';
    const errorMsg = 'Modify resolver failed';

    const customOnSuccess = data => {
      // dispatch({ type: REQUEST_SUCCESS });
      return Promise.all([
        dispatch({ type: RESET, data: data }),
        dispatch(push(`/custom-resolver/manage/${resolveObj.name}/edit`)),
        dispatch(fetchResolvers()),
      ]);
    };
    const customOnError = error => {
      return Promise.all([
        dispatch({ type: MODIFY_RESOLVER_FAIL, data: error }),
      ]);
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
            name: 'custom_resolver',
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
          data[0].headers.forEach(d => {
            headerObj.push({
              name: d.name,
              value: d.value ? d.value : d.value_from_env,
              type: d.value ? 'static' : 'env',
            });
          });
          headerObj.push({
            name: '',
            type: '',
            value: '',
          });
          dispatch({
            type: getHeaderEvents.UPDATE_HEADERS,
            data: [...headerObj],
          });
          return Promise.resolve();
        }
        alert('Resolver not found');
        return Promise.reject();
      },
      error => {
        console.error('Failed to fetch resolver' + JSON.stringify(error));
        dispatch({ type: RESOLVER_FETCH_FAIL, data: error });
        return Promise.reject();
      }
    );
  };
};

const addResolverReducer = (state = addState, action) => {
  switch (action.type) {
    case MANUAL_URL_CHANGED:
      return {
        ...state,
        manualUrl: action.data,
        envName: '',
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
        manualUrl: '',
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
        isRequesting: true,
        isError: null,
      };
    case RESOLVER_FETCH_SUCCESS:
      return {
        ...state,
        name: action.data[0].name,
        manualUrl: action.data[0].url,
        envName: action.data[0].url_from_env,
        headers: action.data[0].headers,
        editState: {
          ...state,
          id: action.data[0].id,
          isModify: false,
          originalName: action.data[0].name,
          originalHeaders: action.data[0].headers,
          originalUrl: action.data[0].url,
          originalEnvUrl: action.data[0].url_from_env,
        },
        isRequesting: false,
        isError: null,
      };
    case RESOLVER_FETCH_FAIL:
      return {
        ...state,
        isRequesting: false,
        isError: action.data,
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
};

export default addResolverReducer;
