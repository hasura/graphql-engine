import { setLSItem, getLSItem } from '../../../utils/localStorage';

const LS_DERIVED_MUTATIONS = 'actions:derivedActions';

export const persistAllDerivedActions = allActions => {
  let stringified;
  try {
    stringified = JSON.stringify(allActions);
  } catch (e) {
    stringified = '{}';
  }
  setLSItem(LS_DERIVED_MUTATIONS, stringified);
};

export const getAllPersistedDerivedActions = () => {
  let allActions = getLSItem(LS_DERIVED_MUTATIONS);
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
