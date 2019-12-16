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
import { makeMigrationCall, fetchRoleList } from '../Data/DataActions';
import {
  generateSetCustomTypesQuery,
  generateCreateActionQuery,
  generateDropActionQuery,
  getFetchActionsQuery,
  getFetchCustomTypesQuery,
  getCreateActionPermissionQuery,
  getDropActionPermissionQuery,
} from '../../Common/utils/v1QueryUtils';
import { getConfirmation } from '../../Common/utils/jsUtils';
import {
  generateActionDefinition,
  getStateValidationError,
  getOverlappingTypeConfirmation,
} from './Common/utils';
import { showErrorNotification } from '../Common/Notification';
import { appPrefix } from './constants';
import { push } from 'react-router-redux';
import {
  reformCustomTypes,
  mergeCustomTypes,
  hydrateTypeRelationships,
} from '../../../shared/utils/hasuraCustomTypeUtils';
import {
  getActionDefinitionFromSdl,
  getTypesFromSdl,
} from '../../../shared/utils/sdlUtils';
import {
  setFetching as createActionRequestInProgress,
  unsetFetching as createActionRequestComplete,
} from './Add/reducer';
import { fetchCustomTypes, setCustomTypes } from '../Types/ServerIO';
import {
  setFetching as modifyActionRequestInProgress,
  unsetFetching as modifyActionRequestComplete,
} from './Modify/reducer';

import {
  makeRequest as makePermRequest,
  setRequestSuccess as setPermRequestSuccess,
  setRequestFailure as setPermRequestFailure,
} from './Permissions/reducer';
import { findAction, getActionPermissions } from './utils';
import { getActionPermissionQueries } from './Permissions/utils';

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
  const {
    add: rawState,
    common: { actions: allActions },
  } = getState().actions;
  const { types: existingTypesList } = getState().types;

  const {
    name: actionName,
    arguments: args,
    outputType,
    error: actionDefError,
  } = getActionDefinitionFromSdl(rawState.actionDefinition.sdl);
  if (actionDefError) {
    return dispatch(
      showErrorNotification('Invalid Action Definition', actionDefError)
    );
  }

  const { types, error: typeDefError } = getTypesFromSdl(
    rawState.typeDefinition.sdl
  );
  if (typeDefError) {
    return dispatch(
      showErrorNotification('Invalid Types Definition', typeDefError)
    );
  }

  const state = {
    webhook: rawState.webhook,
    kind: rawState.kind,
    types,
    name: actionName,
    arguments: args,
    outputType,
  };

  const validationError = getStateValidationError(state, existingTypesList);
  if (validationError) {
    return dispatch(showErrorNotification(validationError));
  }

  const typesWithRelationships = hydrateTypeRelationships(
    state.types,
    existingTypesList
  );

  const { types: mergedTypes, overlappingTypenames } = mergeCustomTypes(
    typesWithRelationships,
    existingTypesList
  );

  if (overlappingTypenames) {
    const isOk = getOverlappingTypeConfirmation(
      state.name,
      allActions,
      existingTypesList,
      overlappingTypenames
    );
    if (!isOk) {
      return;
    }
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
  const downQueries = [actionQueryDown, customFieldsQueryDown];

  const migrationName = `create_action_${state.name}`;
  const requestMsg = 'Creating action...';
  const successMsg = 'Created action successfully';
  const errorMsg = 'Creating action failed';
  const customOnSuccess = () => {
    dispatch(fetchActions()).then(() => {
      dispatch(createActionRequestComplete());
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
  const {
    modify: rawState,
    common: { actions: allActions },
  } = getState().actions;
  const { types: existingTypesList } = getState().types;

  const {
    name: actionName,
    arguments: args,
    outputType,
    error: actionDefError,
  } = getActionDefinitionFromSdl(rawState.actionDefinition.sdl);
  if (actionDefError) {
    return dispatch(
      showErrorNotification('Invalid Action Definition', actionDefError)
    );
  }

  const { types, error: typeDefError } = getTypesFromSdl(
    rawState.typeDefinition.sdl
  );
  if (typeDefError) {
    return dispatch(
      showErrorNotification('Invalid Types Definition', typeDefError)
    );
  }

  const state = {
    webhook: rawState.webhook,
    kind: rawState.kind,
    types,
    name: actionName,
    arguments: args,
    outputType,
  };

  const validationError = getStateValidationError(state);

  if (validationError) {
    return dispatch(showErrorNotification(validationError));
  }

  const typesWithRelationships = hydrateTypeRelationships(
    state.types,
    existingTypesList
  );

  const { types: mergedTypes, overlappingTypenames } = mergeCustomTypes(
    typesWithRelationships,
    existingTypesList
  );

  if (overlappingTypenames) {
    const isOk = getOverlappingTypeConfirmation(
      state.name,
      allActions,
      existingTypesList,
      overlappingTypenames
    );
    if (!isOk) {
      return;
    }
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

export const addActionRel = (objectType, successCb) => (dispatch, getState) => {
  const { types } = getState().actions.relationships;
  const { types: existingTypes } = getState().types;

  const customTypesQueryUp = generateSetCustomTypesQuery(
    reformCustomTypes(types)
  );

  const customTypesQueryDown = generateSetCustomTypesQuery(
    reformCustomTypes(existingTypes)
  );

  const upQueries = [customTypesQueryUp];
  const downQueries = [customTypesQueryDown];

  const migrationName = 'add_action_rel'; // TODO: better migration name
  const requestMsg = 'Adding relationship...';
  const successMsg = 'Relationship added successfully';
  const errorMsg = 'Adding relationship failed';
  const customOnSuccess = () => {
    // dispatch(createActionRequestComplete());
    dispatch(fetchCustomTypes());
    successCb();
  };
  const customOnError = () => {
    // dispatch(createActionRequestComplete());
  };
  // dispatch(createActionRequestInProgress());
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

export const saveActionPermission = (successCb, errorCb) => (
  dispatch,
  getState
) => {
  const {
    common: { actions: allActions, currentAction },
    permissions: { permissionEdit },
  } = getState().actions;

  const allPermissions = getActionPermissions(
    findAction(allActions, currentAction)
  );

  const { upQueries, downQueries } = getActionPermissionQueries(
    permissionEdit,
    allPermissions,
    currentAction
  );

  const migrationName = 'save_action_perm';
  const requestMsg = 'Saving permission...';
  const successMsg = 'Permission saved successfully';
  const errorMsg = 'Saving permission failed';

  const customOnSuccess = () => {
    dispatch(fetchActions());
    dispatch(fetchRoleList());
    dispatch(setPermRequestSuccess());
    if (successCb) {
      successCb();
    }
  };
  const customOnError = () => {
    dispatch(setPermRequestFailure());
    if (errorCb) {
      errorCb();
    }
  };

  dispatch(makePermRequest());
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

export const removeActionPermission = (successCb, errorCb) => (
  dispatch,
  getState
) => {
  const isOk = getConfirmation('This will remove the permission for this role');
  if (!isOk) return;

  const {
    common: { currentAction },
    permissions: { permissionEdit },
  } = getState().actions;

  const { role, filter } = permissionEdit;

  const upQuery = getDropActionPermissionQuery(role, currentAction);
  const downQuery = getCreateActionPermissionQuery(
    { role, filter },
    currentAction
  );

  const migrationName = 'removing_action_perm';
  const requestMsg = 'Removing permission...';
  const successMsg = 'Permission removed successfully';
  const errorMsg = 'Removing permission failed';

  const customOnSuccess = () => {
    dispatch(fetchActions());
    dispatch(fetchRoleList());
    dispatch(setPermRequestSuccess());
    if (successCb) {
      successCb();
    }
  };
  const customOnError = () => {
    dispatch(setPermRequestFailure());
    if (errorCb) {
      errorCb();
    }
  };

  dispatch(makePermRequest());
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
