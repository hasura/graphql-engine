const LS_DERIVED_MUTATIONS = 'HASURA_CONSOLE_DERIVED_MUTATIONS';

export const persistAllDerivedMutations = allMutations => {
  let stringified;
  try {
    stringified = JSON.stringify(allMutations);
  } catch (e) {
    stringified = '{}';
  }
  window.localStorage.setItem(LS_DERIVED_MUTATIONS, stringified);
};

export const getAllPersistedDerivedMutations = () => {
  let allMutations = window.localStorage.getItem(LS_DERIVED_MUTATIONS);
  try {
    allMutations = JSON.parse(allMutations);
  } catch (_) {
    allMutations = {};
  }
  return allMutations;
};

export const getPersistedDerivedMutation = actionName => {
  return getAllPersistedDerivedMutations()[actionName];
};

export const persistDerivedMutation = (actionName, parentMutation) => {
  const allMutations = getAllPersistedDerivedMutations();
  allMutations[actionName] = parentMutation;
  window.localStorage.setItem(
    LS_DERIVED_MUTATIONS,
    JSON.stringify(allMutations)
  );
};

export const removePersistedDerivedMutation = actionName => {
  const allMutations = getAllPersistedDerivedMutations();
  delete allMutations[actionName];
  persistAllDerivedMutations(allMutations);
};
