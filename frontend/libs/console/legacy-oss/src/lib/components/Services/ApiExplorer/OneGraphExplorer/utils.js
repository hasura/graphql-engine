import GraphiQLExplorer from 'graphiql-explorer';
import { getLSItem, setLSItem, LS_KEYS } from '../../../../utils/localStorage';

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
