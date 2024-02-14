import {
  getLSItem,
  setLSItem,
  removeLSItem,
  LS_KEYS,
  loadAdminSecretState,
} from '@hasura/console-legacy-ce';
import globals from '../Globals';
export * from '@hasura/console-legacy-ce';

const loadPATState = () => getLSItem(LS_KEYS.consolePersonalAccessToken);

const savePATState = state => {
  setLSItem(LS_KEYS.consolePersonalAccessToken, state);
};

const clearPATState = () => {
  removeLSItem(LS_KEYS.consolePersonalAccessToken);
};

export const getAdminSecret = () =>
  loadAdminSecretState() || globals.adminSecret;

export { loadPATState, savePATState, clearPATState };
