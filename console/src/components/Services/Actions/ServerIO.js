import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import globals from '../../../Globals';
import requestAction from '../../../utils/requestAction';
import dataHeaders from '../Data/Common/Headers';
import {
  LOADING_ACTIONS,
  LOADING_ACTIONS_SUCCESS,
  LOADING_ACTIONS_FAILURE,
} from './reducer';
import { filterInconsistentMetadataObjects } from '../Settings/utils';
import { makeMigrationCall } from '../Data/DataActions';
import {
  generateSetCustomTypesQuery,
  generateCreateActionQuery,
  generateDropActionQuery,
  getFetchActionsQuery,
} from '../../Common/utils/v1QueryUtils';
import {
  generateActionDefinition,
  getStateValidationError,
  sanitiseState,
} from './Common/utils';
import { showErrorNotification } from '../Common/Notification';
import { appPrefix } from './constants';
import { push } from 'react-router-redux';
import { reformCustomTypes, mergeCustomTypes } from '../Types/utils';

export const fetchActions = () => {
  return (dispatch, getState) => {
    const url = Endpoints.getSchema;
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(getFetchActionsQuery()),
    };
    dispatch({ type: LOADING_ACTIONS });
    return dispatch(requestAction(url, options)).then(
      data => {
        let consistentActions = data;
        const { inconsistentObjects } = getState().metadata;

        if (inconsistentObjects.length > 0) {
          consistentActions = filterInconsistentMetadataObjects(
            data,
            inconsistentObjects,
            'actions'
          );
        }

        dispatch({ type: LOADING_ACTIONS_SUCCESS, data: consistentActions });

        return Promise.resolve();
      },
      error => {
        console.error('Failed to load actions' + JSON.stringify(error));
        dispatch({ type: LOADING_ACTIONS_FAILURE, error });
        return Promise.reject();
      }
    );
  };
};

export const createAction = () => (dispatch, getState) => {
  const { add: rawState } = getState().actions;
  const { types: existingTypesList } = getState().types;

  const state = sanitiseState(rawState);

  // TODO generate down migration for custom types

  const validationError = getStateValidationError(state);
  if (validationError) {
    return dispatch(showErrorNotification(validationError));
  }

  const { types: mergedTypes, overlappingTypename } = mergeCustomTypes(
    state.types,
    existingTypesList
  );

  if (overlappingTypename) {
    return dispatch(
      showErrorNotification(
        `A type called "${overlappingTypename}" already exists`
      )
    );
  }

  console.log(mergedTypes);

  const customFieldsQueryUp = generateSetCustomTypesQuery(
    reformCustomTypes(mergedTypes)
  );

  const customFieldsQueryDown = generateSetCustomTypesQuery(
    reformCustomTypes(existingTypesList)
  );

  const actionQueryUp = generateCreateActionQuery(
    state.name,
    generateActionDefinition(state)
  );

  const actionQueryDown = generateDropActionQuery(state.name);

  const upQueries = [customFieldsQueryUp, actionQueryUp];
  const downQueries = [customFieldsQueryDown, actionQueryDown];

  const migrationName = `create_action_${state.name}`;
  const requestMsg = 'Creating action...';
  const successMsg = 'Created action successfully';
  const errorMsg = 'Creating action failed';
  const customOnSuccess = () => {
    dispatch(fetchActions()).then(() => {
      dispatch(
        push(`${globals.urlPrefix}${appPrefix}/manage/${state.name}/modify`)
      );
    });
  };
  const customOnError = () => {};

  makeMigrationCall(
    dispatch,
    getState,
    upQueries,
    downQueries,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};
