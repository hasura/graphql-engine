export const AUTH_STATE = 'AUTH_STATE';

const getCurrentDate = () => new Date().toISOString();

const initialState = {
  code_verifier: '',
  created_at: getCurrentDate(),
  state: '',
};

export const initLS = () => {
  localStorage.setItem(AUTH_STATE, JSON.stringify(initialState));
};

export const getFromLS = () => {
  try {
    return JSON.parse(localStorage.getItem(AUTH_STATE));
  } catch (e) {
    console.error(e);
    initLS();
    return JSON.parse(localStorage.getItem(AUTH_STATE));
  }
};

export const getKeyFromLS = key => {
  const retrieveFromLS = getFromLS();
  return retrieveFromLS[key] || '';
};

export const modifyKey = (key, value) => {
  const newState = {
    ...getFromLS(AUTH_STATE),
    [key]: value,
  };
  localStorage.setItem(AUTH_STATE, JSON.stringify(newState));
};
