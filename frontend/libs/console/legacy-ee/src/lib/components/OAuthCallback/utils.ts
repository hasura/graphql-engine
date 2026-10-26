import { ReactNode } from 'react';
import { getKeyFromLS } from '../../shared/auth/localStorage';

export const validateOauthResponseState = (state) => {
  const stateInStorage = getKeyFromLS('state');
  return state === stateInStorage;
};
export const defaultErrorMessage = {
  error: null as string | null,
  error_description: null as ReactNode,
};
