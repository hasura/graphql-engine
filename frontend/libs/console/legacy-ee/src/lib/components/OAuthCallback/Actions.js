import requestAction from '../../utils/requestAction';
import { getKeyFromLS } from '../Login/localStorage';

import { getOAuthRedirectUrl, getTokenUrl } from '../Login/utils';

import globals from '../../Globals';

const { hasuraClientID, hasuraOAuthScopes } = globals;

export const retrieveIdToken = code => {
  return dispatch => {
    const formData = new FormData();
    formData.append('code', code);
    formData.append('client_id', hasuraClientID);
    formData.append('grant_type', 'authorization_code');
    formData.append('code_verifier', getKeyFromLS('code_verifier'));
    formData.append('redirect_uri', getOAuthRedirectUrl());
    const options = {
      method: 'POST',
      body: formData,
    };
    return dispatch(requestAction(getTokenUrl(), options));
  };
};

export const retrieveByRefreshToken = refreshToken => {
  return dispatch => {
    const formData = new FormData();
    formData.append('refresh_token', refreshToken);
    formData.append('client_id', hasuraClientID);
    formData.append('grant_type', 'refresh_token');
    formData.append('scopes', hasuraOAuthScopes);
    formData.append('redirect_uri', getOAuthRedirectUrl());
    const options = {
      method: 'POST',
      body: formData,
    };
    return dispatch(requestAction(getTokenUrl(), options));
  };
};
