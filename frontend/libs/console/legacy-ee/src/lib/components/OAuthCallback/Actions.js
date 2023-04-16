import requestAction from '../../utils/requestAction';
import { getKeyFromLS } from '../Login/localStorage';

import { getOAuthRedirectUrl } from '../Login/utils';

export const retrieveIdToken = (provider, code) => {
  return dispatch => {
    const options = {
      method: 'POST',
      body: new URLSearchParams({
        grant_type: 'authorization_code',
        client_id: provider.client_id,
        code_verifier: getKeyFromLS('code_verifier'),
        code: code,
        redirect_uri: getOAuthRedirectUrl(),
      }),
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
      },
    };
    return dispatch(requestAction(provider.request_token_url, options));
  };
};

export const retrieveByRefreshToken = (provider, refreshToken) => {
  return dispatch => {
    const options = {
      method: 'POST',
      body: new URLSearchParams({
        grant_type: 'refresh_token',
        client_id: provider.client_id,
        refresh_token: refreshToken,
        redirect_uri: getOAuthRedirectUrl(),
        scope: provider.scope,
      }),
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
      },
    };
    return dispatch(requestAction(provider.request_token_url, options));
  };
};
