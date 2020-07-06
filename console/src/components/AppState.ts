import globals from '../Globals';
import {
  getLSItem,
  setLSItem,
  removeLSItem,
  lsKeys,
} from '../utils/localStorage';

const loadAppState = () =>
  JSON.parse(getLSItem(lsKeys.consoleLocalInfo) as string);

const saveAppState = (state: string) => {
  setLSItem(lsKeys.consoleLocalInfo, JSON.stringify(state));
};

const loadAdminSecretState = () => getLSItem(lsKeys.consoleAdminSecret);

const saveAdminSecretState = (state: string) => {
  setLSItem(lsKeys.consoleAdminSecret, state);
};

const clearAdminSecretState = () => {
  removeLSItem(lsKeys.consoleAdminSecret);

  globals.adminSecret = null;
};

const clearState = () => removeLSItem(lsKeys.consoleLocalInfo);

export {
  saveAppState,
  saveAdminSecretState,
  loadAppState,
  loadAdminSecretState,
  clearState,
  clearAdminSecretState,
};
