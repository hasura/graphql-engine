/* */
import { listState } from './state';
/* */

import Endpoints, { globalCookiePolicy } from '../../../Endpoints';

import requestAction from '../../../utils/requestAction';

import dataHeaders from '../Data/Common/Headers';

/* Action constants */

const FETCH_RESOLVERS = '@customResolver/FETCH_RESOLVERS';
const RESOLVERS_FETCH_SUCCESS = '@customResolver/RESOLVERS_FETCH_SUCCESS';
const RESOLVERS_FETCH_FAIL = '@customResolver/RESOLVERS_FETCH_FAIL';
const RESET = '@customResolver/RESET';

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
        },
      }),
    };
    dispatch({ type: FETCH_RESOLVERS });
    return dispatch(requestAction(url, options)).then(
      data => {
        return dispatch({ type: RESOLVERS_FETCH_SUCCESS, data: data });
      },
      error => {
        console.error('Failed to load triggers' + JSON.stringify(error));
        return dispatch({ type: RESOLVERS_FETCH_FAIL, data: error });
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
        resolvers: action.data,
        isRequesting: false,
        isError: false,
      };

    case RESOLVERS_FETCH_FAIL:
      return {
        resolvers: [],
        isRequesting: false,
        isError: action.data,
      };
    case RESET:
      return {
        ...listState,
      };
    default:
      return {
        ...state,
      };
  }
};

export { fetchResolvers };
export default listReducer;
