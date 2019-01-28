import globals from 'Globals';

const stateKey = 'CONSOLE_LOCAL_INFO:' + globals.dataApiUrl;
const CONSOLE_ACCESS_KEY = 'CONSOLE_ACCESS_KEY';

const loadAppState = () => JSON.parse(window.localStorage.getItem(stateKey));

const saveAppState = state => {
  window.localStorage.setItem(stateKey, JSON.stringify(state));
};

const loadAccessKeyState = () =>
  window.localStorage.getItem(CONSOLE_ACCESS_KEY);

const saveAccessKeyState = state => {
  window.localStorage.setItem(CONSOLE_ACCESS_KEY, state);
};

const clearAccessKeyState = () => {
  window.localStorage.removeItem(CONSOLE_ACCESS_KEY);
};

const clearState = () => window.localStorage.removeItem(stateKey);

export {
  saveAppState,
  saveAccessKeyState,
  loadAppState,
  loadAccessKeyState,
  clearState,
  clearAccessKeyState,
};
