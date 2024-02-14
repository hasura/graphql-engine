import crypto from 'crypto';

import { v4 as uuidv4 } from 'uuid';

import { getKeyFromLS, modifyKey } from './localStorage';

import globals from '../../Globals';

import { parseQueryString } from '../../helpers/parseQueryString';

import { hasOAuthLoggedIn } from '../OAuthCallback/utils';

const {
  hasuraOAuthUrl,
  hasuraClientID,
  hasuraOAuthScopes,
  relativeOAuthRedirectUrl,
  relativeOAuthTokenUrl,
  urlPrefix,
} = globals;

export const isClientSet = () => {
  if (hasuraClientID && hasuraClientID.length > 0) {
    return true;
  }
  return false;
};

export const getOAuthRedirectUrl = () => {
  let redirectUrl = '';
  if (window && typeof window === 'object') {
    redirectUrl = `${window.location.protocol}//${window.location.host}`;
  }
  redirectUrl = `${redirectUrl}${urlPrefix}${relativeOAuthRedirectUrl}`;
  return redirectUrl;
};

export const getTokenUrl = () => {
  return `${hasuraOAuthUrl}${relativeOAuthTokenUrl}`;
};

const base64URLEncode = (str: Buffer): string => {
  return str
    .toString('base64')
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=/g, '');
};

const sha256 = (str: string): Buffer => {
  return crypto.createHash('sha256').update(str).digest();
};

const generateCodeVerifier = (): string => {
  const codeVerifier = base64URLEncode(crypto.randomBytes(64));
  modifyKey('code_verifier', codeVerifier);
  const codeChallenge = sha256(codeVerifier);
  return base64URLEncode(codeChallenge);
};

const generateState = (): string => {
  const state = uuidv4();
  modifyKey('state', state);
  return state;
};

export const modifyRedirectUrl = (redirectUrl: string): void => {
  modifyKey('redirectUrl', redirectUrl);
};

export const parseQueryParams = (search: string) => {
  return parseQueryString(search);
};

// generalize the oauth authorization url builder with specific identity provider
// the oauth client id needs to be tracked
// so the console can know which token endpoint is used in the callback page
export const getOAuthAuthorizeUrl = (
  url: string,
  clientId: string,
  scope: string
) => {
  const uriObject = new URL(url);
  uriObject.searchParams.set('client_id', clientId);
  uriObject.searchParams.set('response_type', 'code');
  uriObject.searchParams.set('redirect_uri', getOAuthRedirectUrl());
  uriObject.searchParams.set('state', generateState());
  uriObject.searchParams.set('code_challenge_method', 'S256');
  uriObject.searchParams.set('code_challenge', generateCodeVerifier());

  if (scope) {
    uriObject.searchParams.set('scope', scope);
  }

  modifyKey('client_id', clientId);
  return uriObject.toString();
};

export const getAuthorizeUrl = () => {
  const hasuraIdp = getHasuraSsoIdentityProvider();

  if (!hasuraIdp) {
    return false;
  }

  return getOAuthAuthorizeUrl(
    hasuraIdp.authorization_url,
    hasuraIdp.client_id,
    hasuraIdp.scope
  );
};

export const initiateGeneralOAuthRequest = (
  authUrl: string | false,
  location: Location,
  shouldRedirectBack: boolean
) => {
  if (!authUrl) {
    return false;
  }

  const parsed = parseQueryString(location.search);
  hasOAuthLoggedIn(false);
  if (shouldRedirectBack) {
    modifyRedirectUrl(location.pathname);
  } else if (
    'redirect_url' in parsed &&
    parsed['redirect_url'] &&
    parsed['redirect_url'] !== 'undefined' &&
    parsed['redirect_url'] !== 'null'
  ) {
    modifyRedirectUrl(
      Array.isArray(parsed['redirect_url'])
        ? parsed['redirect_url'][0]
        : parsed['redirect_url']
    );
  } else {
    modifyRedirectUrl('/');
  }
  window.location.href = authUrl;

  return true;
};

export const initiateOAuthRequest = (
  location: Location,
  shouldRedirectBack: boolean
) => {
  return initiateGeneralOAuthRequest(
    getAuthorizeUrl(),
    location,
    shouldRedirectBack
  );
};

export const getHasuraSsoIdentityProvider = () => {
  if (!hasuraClientID) {
    return;
  }

  return {
    client_id: hasuraClientID,
    name: 'Hasura Cloud Login',
    scope: hasuraOAuthScopes,
    authorization_url: hasuraOAuthUrl + '/oauth2/auth',
    request_token_url: getTokenUrl(),
  };
};

// get the current sso identity provider by the client id which is stored in the local storage
export const getCurrentSsoIdentityProvider = () => {
  const clientId = getKeyFromLS('client_id');

  if (!clientId || clientId === globals.hasuraClientID) {
    return getHasuraSsoIdentityProvider();
  }

  return globals.ssoIdentityProviders.find(idp => idp.client_id === clientId);
};

// check if the current user token is a hasura collaborator
export const isHasuraCollaboratorUser = (): boolean => {
  return Boolean(
    globals.hasuraClientID &&
      getKeyFromLS('client_id') === globals.hasuraClientID
  );
};
