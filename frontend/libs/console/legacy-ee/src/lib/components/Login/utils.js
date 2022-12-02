import crypto from 'crypto';

import { v4 as uuidv4 } from 'uuid';

import { modifyKey } from './localStorage';

import globals from '../../Globals';

import { parseQueryString } from '../../helpers/parseQueryString';

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

const base64URLEncode = str => {
  return str
    .toString('base64')
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=/g, '');
};

const sha256 = buffer => {
  return crypto
    .createHash('sha256')
    .update(buffer)
    .digest();
};

const generateCodeVerifier = () => {
  const codeVerifier = base64URLEncode(crypto.randomBytes(64));
  modifyKey('code_verifier', codeVerifier);
  const codeChallenge = sha256(codeVerifier);
  return base64URLEncode(codeChallenge);
};

const generateState = () => {
  const state = uuidv4();
  modifyKey('state', state);
  return state;
};

export const modifyRedirectUrl = redirectUrl => {
  modifyKey('redirectUrl', redirectUrl);
};

export const parseQueryParams = search => {
  return parseQueryString(search);
};

export const getAuthorizeUrl = () => {
  const authorizeUrl =
    hasuraOAuthUrl +
    '/oauth2/auth?' +
    'client_id=' +
    hasuraClientID +
    '&' +
    'response_type=code&' +
    'scope=' +
    hasuraOAuthScopes +
    '&' +
    'redirect_uri=' +
    getOAuthRedirectUrl() +
    '&' +
    'state=' +
    generateState() +
    '&' +
    'code_challenge_method=S256&' +
    'code_challenge=' +
    generateCodeVerifier();
  return authorizeUrl;
};
