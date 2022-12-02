import { getKeyFromLS } from '../components/Login/localStorage';

import { getAuthorizeUrl, modifyRedirectUrl } from '../components/Login/utils';

import { parseQueryString } from '../helpers/parseQueryString';

const initiateOAuthRequest = (parsed, auto_login) => {
  if (auto_login) {
    modifyRedirectUrl('/');
    window.location.href = getAuthorizeUrl();
    return;
  }
  const redirectUrl = decodeURIComponent(parsed.redirect_url);
  if (redirectUrl && redirectUrl !== 'undefined') {
    modifyRedirectUrl(redirectUrl);
  } else {
    modifyRedirectUrl('/');
  }
  window.location.href = getAuthorizeUrl();
};

const preLoginHook = (nextState, replaceState, cb) => {
  let hasOauthLoggedIn = false;
  try {
    hasOauthLoggedIn = getKeyFromLS('has_oauth_logged_in');
  } catch (e) {
    // Do nothing
  }
  const parsed = parseQueryString(location.search);

  const isAutoLogin = ('auto_login' in parsed &&
    (
      parsed.auto_login ||
      parsed.auto_login === 'true'
    )) || false;

  if (hasOauthLoggedIn) {
    initiateOAuthRequest(parsed, isAutoLogin);
  } else if (
    isAutoLogin
  ) {
    initiateOAuthRequest(parsed, isAutoLogin);
    return;
  } else {
    cb();
  }
};

export default preLoginHook;
