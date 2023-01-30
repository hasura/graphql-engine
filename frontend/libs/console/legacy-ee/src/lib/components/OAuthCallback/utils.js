import { getKeyFromLS, modifyKey } from '../Login/localStorage';

export const validateOauthResponseState = state => {
  const stateInStorage = getKeyFromLS('state');
  return state === stateInStorage;
};

export const saveIdToken = token => {
  modifyKey('id_token', token);
};

export const hasOAuthLoggedIn = val => {
  modifyKey('has_oauth_logged_in', val);
};

export const defaultErrorMessage = {
  error: null,
  error_description: null,
};
