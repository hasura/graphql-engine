import { ReactNode } from 'react';
import { getKeyFromLS, modifyKey } from '../../shared/auth/localStorage';

export const validateOauthResponseState = (state) => {
  const stateInStorage = getKeyFromLS('state');
  return state === stateInStorage;
};

export const saveIdToken = (token) => {
  modifyKey('id_token', token);
};

export const defaultErrorMessage = {
  error: null as string | null,
  error_description: null as ReactNode,
};
