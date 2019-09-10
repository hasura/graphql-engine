import globals from 'Globals';
import { SET_ADMIN_SECRET } from './Main/Actions';

const stateKey = 'CONSOLE_LOCAL_INFO:' + globals.dataApiUrl;
const CONSOLE_ADMIN_SECRET = 'CONSOLE_ADMIN_SECRET';

const loadAppState = () => JSON.parse(window.localStorage.getItem(stateKey));

const saveAppState = state => {
  window.localStorage.setItem(stateKey, JSON.stringify(state));
};

const loadAdminSecretState = () =>
  window.localStorage.getItem(CONSOLE_ADMIN_SECRET);

const saveAdminSecretState = state => {
  window.localStorage.setItem(CONSOLE_ADMIN_SECRET, state);
};

const clearAdminSecretState = () => dispatch => {
  window.localStorage.removeItem(CONSOLE_ADMIN_SECRET);

  dispatch({
    type: SET_ADMIN_SECRET,
    data: '',
  });
};

const clearState = () => window.localStorage.removeItem(stateKey);

export {
  saveAppState,
  saveAdminSecretState,
  loadAppState,
  loadAdminSecretState,
  clearState,
  clearAdminSecretState,
  CONSOLE_ADMIN_SECRET,
};
