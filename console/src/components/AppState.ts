import globals from '../Globals';
import { getLSItem, setLSItem, removeLSItem } from '../utils/localStorageUtils';

const stateKey = `CONSOLE_LOCAL_INFO:${globals.dataApiUrl}`;

const CONSOLE_ADMIN_SECRET = 'CONSOLE_ADMIN_SECRET';

const loadAppState = () => JSON.parse(getLSItem(stateKey) as string);

const saveAppState = (state: string) => {
  setLSItem(stateKey, JSON.stringify(state));
};

const loadAdminSecretState = () => getLSItem(CONSOLE_ADMIN_SECRET);

const saveAdminSecretState = (state: string) => {
  setLSItem(CONSOLE_ADMIN_SECRET, state);
};

const clearAdminSecretState = () => {
  removeLSItem(CONSOLE_ADMIN_SECRET);

  globals.adminSecret = null;
};

const clearState = () => removeLSItem(stateKey);

export {
  saveAppState,
  saveAdminSecretState,
  loadAppState,
  loadAdminSecretState,
  clearState,
  clearAdminSecretState,
  CONSOLE_ADMIN_SECRET,
};
