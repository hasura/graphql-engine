import { setLSItem, getLSItem, LS_KEYS } from '../../../utils/localStorage';

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
  let allActions = getLSItem(LS_KEYS.derivedActions);
  if (allActions) {
    try {
      allActions = JSON.parse(allActions);
    } catch (_) {
      allActions = {};
    }
  } else {
    allActions = {};
  }
  return allActions;
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
