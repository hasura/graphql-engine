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
  getUpdateActionQuery,
} from '../../Common/utils/v1QueryUtils';
import {
  injectTypeRelationship,
  removeTypeRelationship,
  validateRelTypename,
} from './Relationships/utils';
import { getConfirmation } from '../../Common/utils/jsUtils';
import {
  generateActionDefinition,
  getStateValidationError,
  getOverlappingTypeConfirmation,
} from './Common/utils';
import { showErrorNotification } from '../Common/Notification';
import {
  removePersistedDerivedAction,
  persistDerivedAction,
  updatePersistedDerivation,
} from './lsUtils';
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
import { getActionPermissionMigration } from './Permissions/utils';
import Migration from '../../../utils/migration/Migration';

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
    comment: actionDescription,
    type: actionType,
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
    handler: rawState.handler,
    kind: rawState.kind,
    types,
    actionType,
    name: actionName,
    arguments: args,
    outputType,
    headers: rawState.headers,
    forwardClientHeaders: rawState.forwardClientHeaders,
    comment: actionDescription,
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
  // Migration queries start
  const migration = new Migration();

  const customFieldsQueryUp = generateSetCustomTypesQuery(
    reformCustomTypes(mergedTypes)
  );

  const customFieldsQueryDown = generateSetCustomTypesQuery(
    reformCustomTypes(existingTypesList)
  );
  migration.add(customFieldsQueryUp, customFieldsQueryDown);

  const actionQueryUp = generateCreateActionQuery(
    state.name,
    generateActionDefinition(state),
    actionDescription
  );

  const actionQueryDown = generateDropActionQuery(state.name);

  migration.add(actionQueryUp, actionQueryDown);
  // Migration queries end

  const migrationName = `create_action_${state.name}`;
  const requestMsg = 'Creating action...';
  const successMsg = 'Created action successfully';
  const errorMsg = 'Creating action failed';
  const customOnSuccess = () => {
    if (rawState.derive.operation) {
      persistDerivedAction(state.name, rawState.derive.operation);
    }
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
    migration.upMigration,
    migration.downMigration,
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

  const {
    name: actionName,
    arguments: args,
    outputType,
    type: actionType,
    error: actionDefError,
    comment: actionDescription,
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
    handler: rawState.handler,
    kind: rawState.kind,
    types,
    actionType,
    name: actionName,
    arguments: args,
    outputType,
    headers: rawState.headers,
    forwardClientHeaders: rawState.forwardClientHeaders,
    comment: actionDescription,
  };

  const validationError = getStateValidationError(state);

  if (validationError) {
    return dispatch(showErrorNotification(validationError));
  }

  const typesWithRelationships = hydrateTypeRelationships(
    state.types,
    existingTypesList
  );

  const { types: mergedTypes } = mergeCustomTypes(
    typesWithRelationships,
    existingTypesList
  );

  const isActionNameChange = currentAction.action_name !== state.name;

  const customFieldsQueryUp = generateSetCustomTypesQuery(
    reformCustomTypes(mergedTypes)
  );

  const customFieldsQueryDown = generateSetCustomTypesQuery(
    reformCustomTypes(existingTypesList)
  );

  const dropCurrentActionQuery = generateDropActionQuery(
    currentAction.action_name
  );

  const updateCurrentActionQuery = getUpdateActionQuery(
    generateActionDefinition(state),
    currentAction.action_name,
    actionDescription
  );
  const rollbackActionQuery = getUpdateActionQuery(
    currentAction.action_defn,
    currentAction.action_name,
    currentAction.comment
  );

  const createNewActionQuery = generateCreateActionQuery(
    state.name,
    generateActionDefinition(state),
    actionDescription
  );

  const actionQueryDown = generateDropActionQuery(state.name);
  const oldActionQueryUp = generateCreateActionQuery(
    currentAction.action_name,
    currentAction.action_defn,
    currentAction.comment
  );

  // Migration queries start
  const migration = new Migration();
  if (!isActionNameChange) {
    migration.add(customFieldsQueryUp, customFieldsQueryDown);
    migration.add(updateCurrentActionQuery, rollbackActionQuery);
  } else {
    const isOk = getConfirmation(
      'You seem to have changed the action name. This will cause the permissions to be dropped.'
    );
    if (!isOk) return;
    migration.add(dropCurrentActionQuery, oldActionQueryUp);
    migration.add(customFieldsQueryUp, customFieldsQueryDown);
    migration.add(createNewActionQuery, actionQueryDown);
  }

  const migrationName = `modify_action_${currentAction.action_name}_to_${state.name}`;
  const requestMsg = 'Saving action...';
  const successMsg = 'Action saved successfully';
  const errorMsg = 'Saving action failed';
  const customOnSuccess = () => {
    dispatch(modifyActionRequestComplete());
    if (isActionNameChange) {
      updatePersistedDerivation(currentAction.action_name, state.name);
      const newHref = window.location.href.replace(
        `/manage/${currentAction.action_name}/modify`,
        `/manage/${state.name}/modify`
      );
      window.location.replace(newHref);
    } else {
      dispatch(fetchActions());
    }
  };
  const customOnError = () => {
    dispatch(modifyActionRequestComplete());
  };

  dispatch(modifyActionRequestInProgress());
  makeMigrationCall(
    dispatch,
    getState,
    migration.upMigration,
    migration.downMigration,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};

export const deleteAction = currentAction => (dispatch, getState) => {
  const confirmMessage = `This will permanently delete the action "${currentAction.action_name}" from this table`;
  const isOk = getConfirmation(confirmMessage, true, currentAction.action_name);
  if (!isOk) {
    return;
  }

  // Migration queries start
  const migration = new Migration();

  migration.add(
    generateDropActionQuery(currentAction.action_name),
    generateCreateActionQuery(
      currentAction.action_name,
      currentAction.action_defn,
      currentAction.comment
    )
  );

  const migrationName = `delete_action_${currentAction.action_name}`;
  const requestMsg = 'Deleting action...';
  const successMsg = 'Action deleted successfully';
  const errorMsg = 'Deleting action failed';
  const customOnSuccess = () => {
    dispatch(modifyActionRequestComplete());
    dispatch(push(`${globals.urlPrefix}${appPrefix}/manage`));
    dispatch(fetchActions());
    removePersistedDerivedAction(currentAction.action_name);
  };
  const customOnError = () => {
    dispatch(modifyActionRequestComplete());
  };

  dispatch(modifyActionRequestInProgress());
  makeMigrationCall(
    dispatch,
    getState,
    migration.upMigration,
    migration.downMigration,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};

export const addActionRel = (relConfig, successCb, existingRelConfig) => (
  dispatch,
  getState
) => {
  const { types: existingTypes } = getState().types;

  let typesWithRels = [...existingTypes];

  let validationError;

  if (existingRelConfig) {
    // modifying existing relationship
    // if the relationship is being renamed
    if (existingRelConfig.name !== relConfig.name) {
      // validate the new name
      validationError = validateRelTypename(
        existingTypes,
        relConfig.typename,
        relConfig.name
      );
      // remove old relationship from types
      typesWithRels = removeTypeRelationship(
        existingTypes,
        relConfig.typename,
        existingRelConfig.name
      );
    }
  } else {
    // creating a new relationship

    // validate the relationship name
    validationError = validateRelTypename(
      existingTypes,
      relConfig.typename,
      relConfig.name
    );
  }

  const errorMsg = 'Saving relationship failed';
  if (validationError) {
    return dispatch(showErrorNotification(errorMsg, validationError));
  }

  // add modified relationship to types
  typesWithRels = injectTypeRelationship(
    typesWithRels,
    relConfig.typename,
    relConfig
  );

  const customTypesQueryUp = generateSetCustomTypesQuery(
    reformCustomTypes(typesWithRels)
  );

  const customTypesQueryDown = generateSetCustomTypesQuery(
    reformCustomTypes(existingTypes)
  );
  // Migration queries start
  const migration = new Migration();
  migration.add(customTypesQueryUp, customTypesQueryDown);

  const migrationName = `save_rel_${relConfig.name}_on_${relConfig.typename}`;
  const requestMsg = 'Saving relationship...';
  const successMsg = 'Relationship saved successfully';
  const customOnSuccess = () => {
    // dispatch(createActionRequestComplete());
    dispatch(fetchCustomTypes());
    if (successCb) {
      successCb();
    }
  };

  const customOnError = () => {
    // dispatch(createActionRequestComplete());
  };
  // dispatch(createActionRequestInProgress());
  makeMigrationCall(
    dispatch,
    getState,
    migration.upMigration,
    migration.downMigration,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};

export const removeActionRel = (relName, typename, successCb) => (
  dispatch,
  getState
) => {
  const confirmation = getConfirmation(
    `This will remove the relationship "${relName}" from type "${typename}". This will affect all the actions that use the type "${typename}"`
  );
  if (!confirmation) {
    return;
  }

  const { types: existingTypes } = getState().types;

  const typesWithoutRel = removeTypeRelationship(
    existingTypes,
    typename,
    relName
  );

  const customTypesQueryUp = generateSetCustomTypesQuery(
    reformCustomTypes(typesWithoutRel)
  );

  const customTypesQueryDown = generateSetCustomTypesQuery(
    reformCustomTypes(existingTypes)
  );
  const migration = new Migration();
  migration.add(customTypesQueryUp, customTypesQueryDown);

  const migrationName = 'remove_action_rel'; // TODO: better migration name
  const requestMsg = 'Removing relationship...';
  const successMsg = 'Relationship removed successfully';
  const errorMsg = 'Removing relationship failed';
  const customOnSuccess = () => {
    // dispatch(createActionRequestComplete());
    dispatch(fetchCustomTypes());
    if (successCb) {
      successCb();
    }
  };

  const customOnError = () => {
    // dispatch(createActionRequestComplete());
  };
  // dispatch(createActionRequestInProgress());
  makeMigrationCall(
    dispatch,
    getState,
    migration.upMigration,
    migration.downMigration,
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

  const migration = getActionPermissionMigration(
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
    migration.upMigration,
    migration.downMigration,
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

  const migration = new Migration();
  migration.add(
    getDropActionPermissionQuery(role, currentAction),
    getCreateActionPermissionQuery({ role, filter }, currentAction)
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
    migration.upMigration,
    migration.downMigration,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};
