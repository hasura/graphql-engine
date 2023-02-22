import { programmaticallyTraceError } from '../../../../features/Analytics';
import GraphiQLExplorer from 'graphiql-explorer';
import { getLSItem, setLSItem, LS_KEYS } from '../../../../utils/localStorage';
import { setForceIntrospectAt, setGraphiQLQuery } from '../Actions';

export const makeDefaultArg = () => {
  return false;
};

export const getDefaultScalarArgValue = (parentField, arg, argType) => {
  return GraphiQLExplorer.defaultValue(argType);
};

export const getExplorerWidth = () => {
  const defaultWidth = 300;

  const widthLSRaw = getLSItem(LS_KEYS.oneGraphExplorerWidth);
  const widthLS = parseInt(widthLSRaw, 10);

  return !isNaN(widthLS) ? widthLS : defaultWidth;
};

export const setExplorerWidth = width => {
  setLSItem(LS_KEYS.oneGraphExplorerWidth, width);
};

export const getExplorerIsOpen = () => {
  const defaultIsOpen = true;

  const isOpen = getLSItem(LS_KEYS.oneGraphExplorerOpen);

  return isOpen ? isOpen === 'true' : defaultIsOpen;
};

export const setExplorerIsOpen = isOpen => {
  setLSItem(LS_KEYS.oneGraphExplorerOpen, isOpen);
};

export const persistCodeExporterOpen = isOpen => {
  setLSItem(LS_KEYS.oneGraphExplorerCodeExporterOpen, JSON.stringify(isOpen));
};

export const getPersistedCodeExporterOpen = () => {
  const isOpen = getLSItem(LS_KEYS.oneGraphExplorerCodeExporterOpen);

  if (!isOpen) return false;

  try {
    return JSON.parse(isOpen);
  } catch {
    return false;
  }
};

// Simulates the Run button click on the GraphiQL editor
export const clickRunQueryButton = () => {
  const runQueryButton = document.getElementsByClassName('execute-button');

  // trigger click
  if (runQueryButton && runQueryButton[0]) {
    runQueryButton[0].click();
  } else {
    const error = new Error(
      'Could not find run query button in the DOM (.execute-button)'
    );
    programmaticallyTraceError(error);
  }
};

// ATTENTION: use with care -- this function forces introspection in graphiql
export const forceGraphiQLIntrospection = dispatch => {
  dispatch(setForceIntrospectAt(new Date().getTime().toString()));
};

// ATTENTION: use with care -- this function replaces the existing query in graphiql
export const forceChangeGraphiqlQuery = (query, dispatch) => {
  dispatch(setGraphiQLQuery(query));
};
