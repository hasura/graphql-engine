import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import requestAction from '../../../utils/requestAction';
import dataHeaders from '../Data/Common/Headers';
import { makeMigrationCall } from '../Data/DataActions';
import {
  LOADING_TYPES,
  LOADING_TYPES_FAILURE,
  LOADING_TYPES_SUCCESS,
} from './reducer';
import {
  parseCustomTypes,
  reformCustomTypes,
  hydrateTypeRelationships,
} from '../../../shared/utils/hasuraCustomTypeUtils';
import {
  getFetchCustomTypesQuery,
  generateSetCustomTypesQuery,
} from '../../Common/utils/v1QueryUtils';
import { getConfirmation } from '../../Common/utils/jsUtils';

export const setCustomTypes = (dispatch, response) => {
  dispatch({
    type: LOADING_TYPES_SUCCESS,
    types: response.length ? parseCustomTypes(response[0].custom_types) : [],
  });
};

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
      setCustomTypes(dispatch, data);
      return Promise.resolve();
    },
    error => {
      console.error('Failed to load custom types' + JSON.stringify(error));
      dispatch({ type: LOADING_TYPES_FAILURE, error });
      return Promise.reject();
    }
  );
};

export const setCustomGraphQLTypes = (types, successCb, errorCb) => (
  dispatch,
  getState
) => {
  const isOk = getConfirmation(
    'This could have an effect on the dependent actions.'
  );
  if (!isOk) {
    successCb();
    return;
  }

  const existingTypes = getState().types.types;

  const hydratedTypes = hydrateTypeRelationships(types, existingTypes);

  const upQuery = generateSetCustomTypesQuery(reformCustomTypes(hydratedTypes));
  const downQuery = generateSetCustomTypesQuery(
    reformCustomTypes(existingTypes)
  );

  const migrationName = 'set_custom_types';
  const requestMsg = 'Setting custom types...';
  const successMsg = 'Setting custom types successfull';
  const errorMsg = 'Setting custom types failed';

  const customOnSuccess = () => {
    dispatch(fetchCustomTypes()).then(() => {
      if (successCb) {
        successCb();
      }
    });
  };
  const customOnError = () => {
    if (errorCb) {
      errorCb();
    }
  };

  makeMigrationCall(
    dispatch,
    getState,
    [upQuery],
    [downQuery],
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};
