import GraphiQLExplorer from 'graphiql-explorer';

export const makeDefaultArg = () => {
  return false;
};

export const getDefaultScalarArgValue = (parentField, arg, argType) => {
  return GraphiQLExplorer.defaultValue(argType);
};

export const getExplorerWidthFromLocalStorage = () => {
  const val = parseInt(
    window.localStorage.getItem('graphiql:explorerWidth'),
    10
  );
  return isNaN(val) ? 350 : val;
};

export const setExplorerWidthInLocalStorage = width => {
  localStorage.setItem('graphiql:explorerWidth', width);
};
