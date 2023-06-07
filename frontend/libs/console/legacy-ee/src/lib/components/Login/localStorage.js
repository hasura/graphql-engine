import { getLSItem, setLSItem, LS_KEYS } from '@hasura/console-legacy-ce';

const getCurrentDate = () => new Date().toISOString();

const initialState = {
  client_id: '',
  code_verifier: '',
  created_at: getCurrentDate(),
  state: '',
};

export const initLS = () => {
  setLSItem(LS_KEYS.authState, JSON.stringify(initialState));
};

export const getFromLS = () => {
  try {
    return JSON.parse(getLSItem(LS_KEYS.authState));
  } catch (e) {
    console.error(e);
    initLS();
    return JSON.parse(getLSItem(LS_KEYS.authState));
  }
};

export const getKeyFromLS = key => {
  const retrieveFromLS = getFromLS();
  return retrieveFromLS?.[key] || '';
};

export const modifyKey = (key, value) => {
  const newState = {
    ...getFromLS(LS_KEYS.authState),
    [key]: value,
  };
  setLSItem(LS_KEYS.authState, JSON.stringify(newState));
};
