/* */
import { listState } from './state';
/* */

import Endpoints, { globalCookiePolicy } from '../../../Endpoints';

import requestAction from '../../../utils/requestAction';

import dataHeaders from '../Data/Common/Headers';

/* Action constants */

const FETCH_RESOLVERS = '@customResolver/FETCH_RESOLVERS';
const RESOLVERS_FETCH_SUCCESS = '@customResolver/RESOLVERS_FETCH_SUCCESS';
const FILTER_RESOLVER = '@customResolver/FILTER_RESOLVER';
const RESOLVERS_FETCH_FAIL = '@customResolver/RESOLVERS_FETCH_FAIL';
const RESET = '@customResolver/RESET';

const VIEW_RESOLVER = '@customResolver/VIEW_RESOLVER';

/* */

const fetchResolvers = () => {
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
          order_by: [{ column: 'name', type: 'asc', nulls: 'last' }],
        },
      }),
    };
    dispatch({ type: FETCH_RESOLVERS });
    return dispatch(requestAction(url, options)).then(
      data => {
        dispatch({ type: RESOLVERS_FETCH_SUCCESS, data: data });
        return Promise.resolve();
      },
      error => {
        console.error('Failed to load triggers' + JSON.stringify(error));
        dispatch({ type: RESOLVERS_FETCH_FAIL, data: error });
        return Promise.reject();
      }
    );
  };
};

const listReducer = (state = listState, action) => {
  switch (action.type) {
    case FETCH_RESOLVERS:
      return {
        ...state,
        isRequesting: true,
        isError: false,
      };

    case RESOLVERS_FETCH_SUCCESS:
      return {
        ...state,
        resolvers: action.data,
        isRequesting: false,
        isError: false,
      };

    case RESOLVERS_FETCH_FAIL:
      return {
        ...state,
        resolvers: [],
        isRequesting: false,
        isError: action.data,
      };
    case FILTER_RESOLVER:
      return {
        ...state,
        ...action.data,
      };
    case RESET:
      return {
        ...listState,
      };
    case VIEW_RESOLVER:
      return {
        ...state,
        viewResolver: action.data,
      };
    default:
      return {
        ...state,
      };
  }
};

export { fetchResolvers, FILTER_RESOLVER, VIEW_RESOLVER };
export default listReducer;
