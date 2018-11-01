/* defaultState */
import { addState } from '../state';
/* */

import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import requestAction from '../../../../utils/requestAction';
import dataHeaders from '../../Data/Common/Headers';
import { push } from 'react-router-redux';
import { fetchResolvers } from '../customActions';

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
/* */

const addResolver = () => {
  return (dispatch, getState) => {
    const currState = getState().customResolverData.addData;
    const url = Endpoints.getSchema;
    const resolveObj = {
      name: currState.name,
      url: currState.manualUrl,
      url_from_env: currState.envName,
      headers: [],
    };
    if (resolveObj.url) {
      delete resolveObj.url_from_env;
    } else {
      delete resolveObj.url;
    }
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify({
        type: 'add_custom_resolver',
        args: {
          ...resolveObj,
        },
      }),
    };
    dispatch({ type: ADDING_RESOLVER });
    return dispatch(requestAction(url, options)).then(
      data => {
        return Promise.all([
          dispatch({ type: RESET, data: data }),
          dispatch(push(`/custom-resolver/manage/${resolveObj.name}/edit`)),
          dispatch(fetchResolvers()),
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
  };
};

const deleteResolver = () => {
  return (dispatch, getState) => {
    const currState = getState().customResolverData.addData;
    const url = Endpoints.getSchema;
    const resolveObj = {
      name: currState.editState.originalName,
    };
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify({
        type: 'delete_custom_resolver',
        args: {
          ...resolveObj,
        },
      }),
    };
    dispatch({ type: DELETING_RESOLVER });
    return dispatch(requestAction(url, options)).then(
      data => {
        return Promise.all([
          dispatch({ type: RESET, data: data }),
          dispatch(push('/custom-resolver')),
          dispatch(fetchResolvers()),
          Promise.resolve(),
        ]);
      },
      error => {
        console.error('Failed to delete triggers' + JSON.stringify(error));
        dispatch({ type: DELETE_RESOLVER_FAIL, data: error });
        alert(JSON.stringify(error));
        return Promise.reject();
      }
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
    default:
      return {
        ...state,
      };
  }
};

export { inputChange, addResolver, fetchResolver, deleteResolver, RESET };

export default addResolverReducer;
