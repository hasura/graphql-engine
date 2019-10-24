import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import requestAction from '../../../utils/requestAction';
import dataHeaders from '../Data/Common/Headers';
import {
  LOADING_TYPES,
  LOADING_TYPES_FAILURE,
  LOADING_TYPES_SUCCESS,
} from './reducer';
import { parseCustomTypes } from './utils';
import { getFetchCustomTypesQuery } from '../../Common/utils/v1QueryUtils';

export const fetchCustomTypes = () => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(getFetchCustomTypesQuery()),
  };
  dispatch({ type: LOADING_TYPES });
  return dispatch(requestAction(url, options)).then(
    data => {
      if (data.length) {
        dispatch({
          type: LOADING_TYPES_SUCCESS,
          types: parseCustomTypes(data[0].custom_types),
        });
      } else {
        dispatch({ type: LOADING_TYPES_SUCCESS });
      }
      return Promise.resolve();
    },
    error => {
      console.error('Failed to load custom types' + JSON.stringify(error));
      dispatch({ type: LOADING_TYPES_FAILURE, error });
      return Promise.reject();
    }
  );
};
