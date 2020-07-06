import GraphiQLExplorer from 'graphiql-explorer';
import { getLSItem, setLSItem, lsKeys } from '../../../../utils/localStorage';

export const makeDefaultArg = () => {
  return false;
};

export const getDefaultScalarArgValue = (parentField, arg, argType) => {
  return GraphiQLExplorer.defaultValue(argType);
};

export const getExplorerWidth = () => {
  const defaultWidth = 300;

  const widthLSRaw = getLSItem(lsKeys.oneGraphExplorerWidth);
  const widthLS = parseInt(widthLSRaw, 10);

  return !isNaN(widthLS) ? widthLS : defaultWidth;
};

export const setExplorerWidth = width => {
  setLSItem(lsKeys.oneGraphExplorerWidth, width);
};

export const getExplorerIsOpen = () => {
  const defaultIsOpen = true;

  const isOpen = getLSItem(lsKeys.oneGraphExplorerOpen);

  return isOpen ? isOpen === 'true' : defaultIsOpen;
};

export const setExplorerIsOpen = isOpen => {
  setLSItem(lsKeys.oneGraphExplorerOpen, isOpen);
};

export const persistCodeExporterOpen = isOpen => {
  setLSItem(lsKeys.oneGraphExplorerCodeExporterOpen, JSON.stringify(isOpen));
};

export const getPersistedCodeExporterOpen = () => {
  const isOpen = getLSItem(lsKeys.oneGraphExplorerCodeExporterOpen);

  if (!isOpen) return false;

  try {
    return JSON.parse(isOpen);
  } catch {
    return false;
  }
};
