import globals from '../Globals';
import {
  getLSItem,
  setLSItem,
  removeLSItem,
  LS_KEYS,
  getParsedLSItem,
} from '../utils/localStorage';

const loadAppState = () => {
  return getParsedLSItem(LS_KEYS.consoleLocalInfo);
};

const saveAppState = (state: string) => {
  setLSItem(LS_KEYS.consoleLocalInfo, JSON.stringify(state));
};

const loadAdminSecretState = () => getLSItem(LS_KEYS.consoleAdminSecret);

const saveAdminSecretState = (state: string) => {
  setLSItem(LS_KEYS.consoleAdminSecret, state);
};

const clearAdminSecretState = () => {
  removeLSItem(LS_KEYS.consoleAdminSecret);

  globals.adminSecret = null;
};

const clearState = () => removeLSItem(LS_KEYS.consoleLocalInfo);

export {
  saveAppState,
  saveAdminSecretState,
  loadAppState,
  loadAdminSecretState,
  clearState,
  clearAdminSecretState,
};
