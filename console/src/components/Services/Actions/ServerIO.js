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
  getFetchCustomTypesQuery,
} from '../../Common/utils/v1QueryUtils';
import { getConfirmation } from '../../Common/utils/jsUtils';
import {
  generateActionDefinition,
  getStateValidationError,
  sanitiseState,
} from './Common/utils';
import { showErrorNotification } from '../Common/Notification';
import { appPrefix } from './constants';
import { push } from 'react-router-redux';
import { reformCustomTypes, mergeCustomTypes } from '../Types/utils';
import {
  setFetching as createActionRequestInProgress,
  unsetFetching as createActionRequestComplete,
} from './Add/reducer';
import { setCustomTypes } from '../Types/ServerIO';
import {
  setFetching as modifyActionRequestInProgress,
  unsetFetching as modifyActionRequestComplete,
} from './Modify/reducer';

export const fetchActions = () => {
  return (dispatch, getState) => {
    const url = Endpoints.getSchema;
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify({
        type: 'bulk',
        args: [getFetchActionsQuery(), getFetchCustomTypesQuery()],
      }),
    };
    dispatch({ type: LOADING_ACTIONS });
    return dispatch(requestAction(url, options)).then(
      data => {
        setCustomTypes(dispatch, data[1]);

        let consistentActions = data[0];
        const { inconsistentObjects } = getState().metadata;

        if (inconsistentObjects.length > 0) {
          consistentActions = filterInconsistentMetadataObjects(
            data[0],
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
    dispatch(createActionRequestComplete());
    dispatch(fetchActions()).then(() => {
      dispatch(
        push(`${globals.urlPrefix}${appPrefix}/manage/${state.name}/modify`)
      );
    });
  };
  const customOnError = () => {
    dispatch(createActionRequestComplete());
  };
  dispatch(createActionRequestInProgress());
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

export const saveAction = currentAction => (dispatch, getState) => {
  const { modify: rawState } = getState().actions;
  const { types: existingTypesList } = getState().types;

  const state = sanitiseState(rawState);

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

  const customFieldsQueryUp = generateSetCustomTypesQuery(
    reformCustomTypes(mergedTypes)
  );

  const customFieldsQueryDown = generateSetCustomTypesQuery(
    reformCustomTypes(existingTypesList)
  );

  const dropCurrentActionQuery = generateDropActionQuery(
    currentAction.action_name
  );
  const actionQueryUp = generateCreateActionQuery(
    state.name,
    generateActionDefinition(state)
  );

  const actionQueryDown = generateDropActionQuery(state.name);
  const oldActionQueryUp = generateCreateActionQuery(
    currentAction.action_name,
    currentAction.action_defn
  );

  const upQueries = [
    dropCurrentActionQuery,
    customFieldsQueryUp,
    actionQueryUp,
  ];
  const downQueries = [
    actionQueryDown,
    customFieldsQueryDown,
    oldActionQueryUp,
  ];

  const migrationName = `modify_action_${currentAction.action_name}_to_${
    state.name
  }`;
  const requestMsg = 'Saving action...';
  const successMsg = 'Action saved successfully';
  const errorMsg = 'Saving action failed';
  const customOnSuccess = () => {
    dispatch(modifyActionRequestComplete());
    const newHref = window.location.href.replace(
      `/manage/${currentAction.action_name}/modify`,
      `/manage/${state.name}/modify`
    );
    window.location.replace(newHref);
  };
  const customOnError = () => {
    dispatch(modifyActionRequestComplete());
  };

  dispatch(modifyActionRequestInProgress());
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

export const deleteAction = currentAction => (dispatch, getState) => {
  const confirmMessage = `This will permanently delete the action "${
    currentAction.action_name
  }" from this table`;
  const isOk = getConfirmation(confirmMessage, true, currentAction.action_name);
  if (!isOk) {
    return;
  }
  const upQuery = generateDropActionQuery(currentAction.action_name);
  const downQuery = generateCreateActionQuery(
    currentAction.action_name,
    currentAction.action_defn
  );

  const migrationName = `delete_action_${currentAction.action_name}`;
  const requestMsg = 'Deleting action...';
  const successMsg = 'Action deleted successfully';
  const errorMsg = 'Deleting action failed';
  const customOnSuccess = () => {
    dispatch(modifyActionRequestComplete());
    dispatch(push(`${globals.urlPrefix}${appPrefix}/manage`));
    dispatch(fetchActions());
  };
  const customOnError = () => {
    dispatch(modifyActionRequestComplete());
  };

  dispatch(modifyActionRequestInProgress());
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
