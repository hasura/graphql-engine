import { getLSItem, setLSItem } from '@hasura/shared/utils';
import { LS_KEYS } from '@hasura/shared/types';

const getCurrentDate = () => new Date().toISOString();

type AuthState = {
  client_id: string;
  code_verifier: string;
  created_at: string;
  state: string;
  redirect_url?: string;
  id_token?: string;
};

const initialState: AuthState = {
  client_id: '',
  code_verifier: '',
  created_at: getCurrentDate(),
  state: '',
};

export const initLS = () => {
  setLSItem(
    LS_KEYS.consoleOAuthLoginSessionState,
    JSON.stringify(initialState),
  );
};

export const getFromLS = (): AuthState => {
  const authState = getLSItem(LS_KEYS.consoleOAuthLoginSessionState);
  if (!authState) {
    initLS();
    return { ...initialState };
  }

  try {
    return JSON.parse(authState);
  } catch (e) {
    console.error(e);
    initLS();
    return { ...initialState };
  }
};

export const getKeyFromLS = <K extends keyof AuthState>(
  key: K,
): AuthState[K] => {
  try {
    const retrieveFromLS = getFromLS();
    return retrieveFromLS?.[key];
  } catch {
    return initialState[key];
  }
};

export const modifyKey = (key: keyof AuthState, value: string) => {
  const newState = {
    ...getFromLS(),
    [key]: value,
  };

  setLSItem(LS_KEYS.consoleOAuthLoginSessionState, JSON.stringify(newState));
};
