import globals from '../../../Globals';
import { makeMigrationCall } from '../Data/DataActions';
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
import {
  setFetching as modifyActionRequestInProgress,
  unsetFetching as modifyActionRequestComplete,
} from './Modify/reducer';

import {
  makePermRequest,
  setRequestSuccess as setPermRequestSuccess,
  setRequestFailure as setPermRequestFailure,
} from './Permissions/reducer';
import { exportMetadata } from '../../../metadata/actions';
import {
  customTypesSelector,
  actionsSelector,
} from '../../../metadata/selector';
import {
  generateSetCustomTypesQuery,
  generateCreateActionQuery,
  generateDropActionQuery,
  getCreateActionPermissionQuery,
  getUpdateActionQuery,
  getDropActionPermissionQuery,
} from '../../../metadata/queryUtils';
import { getActionPermissionMigration } from './Permissions/utils';
import Migration from '../../../utils/migration/Migration';
import {
  findAction,
  getActionPermissions,
  removePersistedDerivedAction,
  persistDerivedAction,
  updatePersistedDerivation,
} from './utils';

export const createAction = () => (dispatch, getState) => {
  const { add: rawState } = getState().actions;
  const existingTypesList = customTypesSelector(getState());
  const allActions = actionsSelector(getState());

  const actionComment = rawState.comment ? rawState.comment.trim() : null;

  const {
    name: actionName,
    arguments: args,
    outputType,
    error: actionDefError,
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
    handler: rawState.handler.trim(),
    kind: rawState.kind,
    types,
    actionType,
    name: actionName,
    arguments: args,
    outputType,
    headers: rawState.headers,
    comment: actionComment,
    timeout: parseInt(rawState.timeout, 10),
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
    actionComment
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
    dispatch(exportMetadata()).then(() => {
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
  const existingTypesList = customTypesSelector(getState());
  const {
    name: actionName,
    arguments: args,
    outputType,
    type: actionType,
    error: actionDefError,
  } = getActionDefinitionFromSdl(rawState.actionDefinition.sdl);

  if (actionDefError) {
    return dispatch(
      showErrorNotification('Invalid Action Definition', actionDefError)
    );
  }

  const actionComment = rawState.comment ? rawState.comment.trim() : null;

  const { types, error: typeDefError } = getTypesFromSdl(
    rawState.typeDefinition.sdl
  );
  if (typeDefError) {
    return dispatch(
      showErrorNotification('Invalid Types Definition', typeDefError)
    );
  }

  const state = {
    handler: rawState.handler.trim(),
    kind: rawState.kind,
    types,
    actionType,
    name: actionName,
    arguments: args,
    outputType,
    headers: rawState.headers,
    forwardClientHeaders: rawState.forwardClientHeaders,
    timeout: parseInt(rawState.timeout, 10),
    comment: actionComment,
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

  const isActionNameChange = currentAction.name !== state.name;

  const customFieldsQueryUp = generateSetCustomTypesQuery(
    reformCustomTypes(mergedTypes)
  );

  const customFieldsQueryDown = generateSetCustomTypesQuery(
    reformCustomTypes(existingTypesList)
  );

  const dropCurrentActionQuery = generateDropActionQuery(currentAction.name);

  const updateCurrentActionQuery = getUpdateActionQuery(
    generateActionDefinition(state),
    currentAction.name,
    actionComment
  );
  const rollbackActionQuery = getUpdateActionQuery(
    currentAction.definition,
    currentAction.name,
    currentAction.comment
  );

  const createNewActionQuery = generateCreateActionQuery(
    state.name,
    generateActionDefinition(state),
    actionComment
  );

  const actionQueryDown = generateDropActionQuery(state.name);
  const oldActionQueryUp = generateCreateActionQuery(
    currentAction.name,
    currentAction.definition,
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

  const migrationName = `modify_action_${currentAction.name}_to_${state.name}`;
  const requestMsg = 'Saving action...';
  const successMsg = 'Action saved successfully';
  const errorMsg = 'Saving action failed';
  const customOnSuccess = () => {
    dispatch(modifyActionRequestComplete());
    if (isActionNameChange) {
      updatePersistedDerivation(currentAction.name, state.name);
      const newHref = window.location.href.replace(
        `/manage/${currentAction.name}/modify`,
        `/manage/${state.name}/modify`
      );
      window.location.replace(newHref);
    } else {
      dispatch(exportMetadata());
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
  const confirmMessage = `This will permanently delete the action "${currentAction.name}" from this table`;
  const isOk = getConfirmation(confirmMessage, true, currentAction.name);
  if (!isOk) {
    return;
  }

  // Migration queries start
  const migration = new Migration();

  migration.add(
    generateDropActionQuery(currentAction.name),
    generateCreateActionQuery(
      currentAction.name,
      currentAction.definition,
      currentAction.comment
    )
  );

  const migrationName = `delete_action_${currentAction.name}`;
  const requestMsg = 'Deleting action...';
  const successMsg = 'Action deleted successfully';
  const errorMsg = 'Deleting action failed';
  const customOnSuccess = () => {
    dispatch(modifyActionRequestComplete());
    dispatch(push(`${globals.urlPrefix}${appPrefix}/manage`));
    dispatch(exportMetadata());
    removePersistedDerivedAction(currentAction.name);
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
  const existingTypes = customTypesSelector(getState());
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

  const customTypesQueryUp = generateSetCustomTypesQuery({
    ...reformCustomTypes(typesWithRels),
    source: relConfig.refDb,
  });

  const customTypesQueryDown = generateSetCustomTypesQuery({
    ...reformCustomTypes(existingTypes),
    source: relConfig.refDb,
  });

  const migration = new Migration();
  migration.add(customTypesQueryUp, customTypesQueryDown);

  const migrationName = `save_rel_${relConfig.name}_on_${relConfig.typename}`;
  const requestMsg = 'Saving relationship...';
  const successMsg = 'Relationship saved successfully';

  makeMigrationCall(
    dispatch,
    getState,
    migration.upMigration,
    migration.downMigration,
    migrationName,
    () => dispatch(exportMetadata(successCb)),
    () => {},
    requestMsg,
    successMsg,
    errorMsg
  );
};

export const removeActionRel = (relName, source, typename, successCb) => (
  dispatch,
  getState
) => {
  const confirmation = getConfirmation(
    `This will remove the relationship "${relName}" from type "${typename}". This will affect all the actions that use the type "${typename}"`
  );
  if (!confirmation) {
    return;
  }

  const existingTypes = customTypesSelector(getState());

  const typesWithoutRel = removeTypeRelationship(
    existingTypes,
    typename,
    relName
  );

  const customTypesQueryUp = generateSetCustomTypesQuery({
    ...reformCustomTypes(typesWithoutRel),
    source,
  });

  const customTypesQueryDown = generateSetCustomTypesQuery({
    ...reformCustomTypes(existingTypes),
    source,
  });
  const migration = new Migration();
  migration.add(customTypesQueryUp, customTypesQueryDown);

  const migrationName = `remove_action_relationship_${relName}_from_${typename}`;
  const requestMsg = `Removing relationship ${relName}...`;
  const successMsg = 'Relationship removed successfully';
  const errorMsg = `Failed to remove the relationship: "${relName}"`;

  makeMigrationCall(
    dispatch,
    getState,
    migration.upMigration,
    migration.downMigration,
    migrationName,
    () => dispatch(exportMetadata(successCb)),
    () => {},
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
    common: { currentAction },
    permissions: { permissionEdit },
  } = getState().actions;
  const allActions = actionsSelector(getState());

  const allPermissions = getActionPermissions(
    findAction(allActions, currentAction)
  );

  const migration = getActionPermissionMigration(
    permissionEdit,
    allPermissions,
    currentAction
  );

  const { role, newRole } = permissionEdit;
  const roleName = (newRole || role).trim();

  const migrationName = `save_action_permission_${currentAction}_${roleName}`;
  const requestMsg = 'Saving permission...';
  const successMsg = 'Permission saved successfully';
  const errorMsg = `Failed to save permissions for role "${roleName}"`;

  const customOnSuccess = () => {
    dispatch(exportMetadata());
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
    dispatch(exportMetadata());
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
