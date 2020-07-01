import globals from 'Globals';

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

const clearAdminSecretState = () => {
  window.localStorage.removeItem(CONSOLE_ADMIN_SECRET);

  globals.adminSecret = null;
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
