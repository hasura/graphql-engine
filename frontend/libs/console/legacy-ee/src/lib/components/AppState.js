import {
  getLSItem,
  setLSItem,
  removeLSItem,
  LS_KEYS,
} from '@hasura/console-legacy-ce';

export * from '@hasura/console-legacy-ce';

const loadPATState = () => getLSItem(LS_KEYS.consolePersonalAccessToken);

const savePATState = state => {
  setLSItem(LS_KEYS.consolePersonalAccessToken, state);
};

const clearPATState = () => {
  removeLSItem(LS_KEYS.consolePersonalAccessToken);
};

export { loadPATState, savePATState, clearPATState };
