import {
  setLSItem,
  LS_KEYS,
  getParsedLSItem,
} from '../../../utils/localStorage';

export const findType = (types, typeName) => {
  return types.find(t => t.name === typeName);
};

export const getActionName = action => {
  return action.name;
};

export const findAction = (actions, actionName) => {
  return actions.find(a => getActionName(a) === actionName);
};

export const getActionOutputType = action => {
  return action.definition.output_type;
};

export const getActionOutputFields = (action, types) => {
  const outputTypeName = getActionOutputType(action);

  const outputType = findType(types, outputTypeName);

  return outputType.fields;
};

export const getActionArguments = action => {
  return action.definition.arguments || [];
};

export const getActionType = action => {
  return action.definition.type;
};

export const getActionPermissions = action => {
  return action.permissions;
};

export const findActionPermission = (perms, role) => {
  return perms.find(p => p.role === role);
};

export const getActionComment = action => action.comment;

export const persistAllDerivedActions = allActions => {
  let stringified;
  try {
    stringified = JSON.stringify(allActions);
  } catch (e) {
    stringified = '{}';
  }
  setLSItem(LS_KEYS.derivedActions, stringified);
};

export const getAllPersistedDerivedActions = () => {
  return getParsedLSItem(LS_KEYS.derivedActions, {});
};

export const getPersistedDerivedAction = actionName => {
  return getAllPersistedDerivedActions()[actionName];
};

export const persistDerivedAction = (actionName, parentOperation) => {
  const allActions = getAllPersistedDerivedActions();
  allActions[actionName] = parentOperation;
  persistAllDerivedActions(allActions);
};

export const removePersistedDerivedAction = actionName => {
  const allActions = getAllPersistedDerivedActions();
  delete allActions[actionName];
  persistAllDerivedActions(allActions);
};

export const updatePersistedDerivation = (oldActionName, newActionName) => {
  const parentOperation = getPersistedDerivedAction(oldActionName);
  if (parentOperation) {
    persistDerivedAction(newActionName, parentOperation);
    removePersistedDerivedAction(oldActionName);
  }
};
