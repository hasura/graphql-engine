import { getKeyFromLS } from '../components/Login/localStorage';

import {
  getCurrentSsoIdentityProvider,
  getHasuraSsoIdentityProvider,
  getOAuthAuthorizeUrl,
  modifyRedirectUrl,
} from '../components/Login/utils';

import { parseQueryString } from '../helpers/parseQueryString';

const initiateOAuthRequest = (parsed, auto_login) => {
  const idp = getCurrentSsoIdentityProvider() || getHasuraSsoIdentityProvider();

  if (!idp) {
    return false;
  }

  const authorizeUrl = getOAuthAuthorizeUrl(
    idp.authorization_url,
    idp.client_id,
    idp.scope
  );

  if (auto_login) {
    modifyRedirectUrl('/');
    window.location.href = authorizeUrl;
    return true;
  }
  const redirectUrl = decodeURIComponent(parsed.redirect_url);
  if (redirectUrl && redirectUrl !== 'undefined') {
    modifyRedirectUrl(redirectUrl);
  } else {
    modifyRedirectUrl('/');
  }
  window.location.href = authorizeUrl;
  return true;
};

const preLoginHook = (nextState, replaceState, cb) => {
  let hasOauthLoggedIn = false;
  try {
    hasOauthLoggedIn = getKeyFromLS('has_oauth_logged_in');
  } catch (e) {
    // Do nothing
  }
  const parsed = parseQueryString(location.search);

  const isAutoLogin =
    ('auto_login' in parsed &&
      (parsed.auto_login || parsed.auto_login === 'true')) ||
    false;

  if (hasOauthLoggedIn || isAutoLogin) {
    // fallback to the login page if the Hasura OAuth client isn't set
    return initiateOAuthRequest(parsed, isAutoLogin) || cb();
  }

  cb();
};

export default preLoginHook;
