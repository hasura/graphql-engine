export * from '@hasura/console-oss/lib/appState';

const CONSOLE_PERSONAL_ACCESS_TOKEN = 'PERSONAL_ACCESS_TOKEN';

const loadPATState = () =>
  window.localStorage.getItem(CONSOLE_PERSONAL_ACCESS_TOKEN);

const savePATState = state => {
  window.localStorage.setItem(CONSOLE_PERSONAL_ACCESS_TOKEN, state);
};

const clearPATState = () => {
  window.localStorage.removeItem(CONSOLE_PERSONAL_ACCESS_TOKEN);
};

export {
  loadPATState,
  savePATState,
  clearPATState,
  CONSOLE_PERSONAL_ACCESS_TOKEN,
};
