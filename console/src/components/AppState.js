import globals from 'Globals';

const stateKey = 'CONSOLE_LOCAL_INFO:' + globals.dataApiUrl;

const loadAppState = () => JSON.parse(window.localStorage.getItem(stateKey));

const saveAppState = state => {
  window.localStorage.setItem(stateKey, JSON.stringify(state));
};

const clearState = () => window.localStorage.removeItem(stateKey);

export { saveAppState, loadAppState, clearState };
