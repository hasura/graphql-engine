import GraphiQLExplorer from 'graphiql-explorer-hasura';

export const makeDefaultArg = () => {
  return false;
};

export const getDefaultScalarArgValue = (parentField, arg, argType) => {
  return GraphiQLExplorer.defaultValue(argType);
};

export const getExplorerWidth = () => {
  const defaultWidth = 300;

  const widthLSRaw = window.localStorage.getItem('graphiql:explorerWidth');
  const widthLS = parseInt(widthLSRaw, 10);

  return !isNaN(widthLS) ? widthLS : defaultWidth;
};

export const setExplorerWidth = width => {
  localStorage.setItem('graphiql:explorerWidth', width);
};

export const getExplorerIsOpen = () => {
  const defaultIsOpen = true;

  const isOpen = window.localStorage.getItem('graphiql:explorerOpen');

  return isOpen ? isOpen === 'true' : defaultIsOpen;
};

export const setExplorerIsOpen = isOpen => {
  window.localStorage.setItem('graphiql:explorerOpen', isOpen);
};
