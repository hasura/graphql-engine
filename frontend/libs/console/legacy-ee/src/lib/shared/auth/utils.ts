import crypto from 'crypto';
import { v4 as uuidv4 } from 'uuid';
import { getKeyFromLS, modifyKey } from './localStorage';
import { parseQueryString } from '../../helpers/parseQueryString';
import { Location } from 'react-router';
import { requestJson } from '@hasura/shared/utils';
import type { Privilege } from '@hasura/console-legacy-ce';
import { EnterpriseAuthState } from './types';
import {
  CONSTANT_HEADERS,
  HASURA_OAUTH_SCOPES,
  RELATIVE_OAUTH_REDIRECT_URL,
  RELATIVE_OAUTH_TOKEN_URL,
} from '../../constants';
import { jwtDecode, JwtPayload } from 'jwt-decode';
import {
  ADMIN_SECRET_HEADER_KEY,
  EnvVars,
  HASURA_COLLABORATOR_TOKEN,
  HASURA_SSO_TOKEN,
  OAuthTokenResponse,
  ProServerEnv,
  SsoIdentityProvider,
} from '@hasura/shared/types';

export const getOAuthRedirectUrl = (urlPrefix: string) => {
  let redirectUrl = '';
  if (window && typeof window === 'object') {
    redirectUrl = `${window.location.protocol}//${window.location.host}`;
  }
  redirectUrl = `${redirectUrl}${urlPrefix}${RELATIVE_OAUTH_REDIRECT_URL}`;

  return redirectUrl;
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

// generalize the oauth authorization url builder with specific identity provider
// the oauth client id needs to be tracked
// so the console can know which token endpoint is used in the callback page
export const getOAuthAuthorizeUrl = (
  url: string,
  clientId: string,
  scope: string,
  redirectUrl: string,
) => {
  const uriObject = new URL(url);
  uriObject.searchParams.set('client_id', clientId);
  uriObject.searchParams.set('response_type', 'code');
  uriObject.searchParams.set('redirect_uri', redirectUrl);
  uriObject.searchParams.set('state', generateState());
  uriObject.searchParams.set('code_challenge_method', 'S256');
  uriObject.searchParams.set('code_challenge', generateCodeVerifier());

  if (scope) {
    uriObject.searchParams.set('scope', scope);
  }

  modifyKey('client_id', clientId);
  return uriObject.toString();
};

export const modifyRedirectUrl = (redirectUrl: string): void => {
  modifyKey('redirect_url', redirectUrl);
};

export const initiateGeneralOAuthRequest = (
  authUrl: string | false,
  location: Location,
  shouldRedirectBack: boolean | undefined,
) => {
  if (!authUrl) {
    return false;
  }

  const parsed = parseQueryString(location.search);
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
        ? (parsed['redirect_url'][0] ?? '/')
        : parsed['redirect_url'],
    );
  } else {
    modifyRedirectUrl('/');
  }
  window.location.href = authUrl;

  return true;
};

export const getHasuraSsoIdentityProvider = (envVars: EnvVars) => {
  if (!envVars.consoleId) {
    return null;
  }

  return {
    client_id: envVars.consoleId,
    name: 'Hasura Cloud Login',
    scope: HASURA_OAUTH_SCOPES,
    authorization_url: envVars.hasuraOAuthUrl + '/oauth2/auth',
    request_token_url: envVars.hasuraOAuthUrl + RELATIVE_OAUTH_TOKEN_URL,
  };
};

const getAuthorizeUrl = (envVars: EnvVars) => {
  const hasuraIdp = getHasuraSsoIdentityProvider(envVars);

  if (!hasuraIdp) {
    return false;
  }

  return getOAuthAuthorizeUrl(
    hasuraIdp.authorization_url,
    hasuraIdp.client_id,
    hasuraIdp.scope,
    getOAuthRedirectUrl(envVars.urlPrefix),
  );
};

export const initiateOAuthRequest = (
  envVars: EnvVars,
  location: Location,
  shouldRedirectBack: boolean | undefined,
) => {
  return initiateGeneralOAuthRequest(
    getAuthorizeUrl(envVars),
    location,
    shouldRedirectBack,
  );
};

// get the current sso identity provider by the client id which is stored in the local storage
export const getCurrentSsoIdentityProvider = (envVars: EnvVars) => {
  const clientId = getKeyFromLS('client_id');

  if (!clientId || clientId === envVars.consoleId) {
    return getHasuraSsoIdentityProvider(envVars);
  }

  return (envVars as ProServerEnv).ssoIdentityProviders?.find(
    (idp) => idp.client_id === clientId,
  );
};

export const sso3rdPartyEnabled = (envVars: EnvVars) =>
  (envVars.consoleType === 'pro' || envVars.consoleType === 'pro-lite') &&
  envVars.consoleMode === 'server' &&
  Array.isArray(envVars.ssoIdentityProviders) &&
  envVars.ssoIdentityProviders.length > 0;

export const retrieveIdToken = (
  provider: SsoIdentityProvider,
  code: string,
  urlPrefix: string,
) => {
  const options = {
    method: 'POST',
    body: new URLSearchParams({
      grant_type: 'authorization_code',
      client_id: provider.client_id,
      code_verifier: getKeyFromLS('code_verifier'),
      code: code,
      redirect_uri: getOAuthRedirectUrl(urlPrefix),
    }),
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
    },
  };

  return requestJson<OAuthTokenResponse>(provider.request_token_url, options);
};

export const retrieveByRefreshToken = (
  provider: SsoIdentityProvider,
  refreshToken: string,
  redirectUri: string,
) => {
  const options = {
    method: 'POST',
    body: new URLSearchParams({
      grant_type: 'refresh_token',
      client_id: provider.client_id,
      refresh_token: refreshToken,
      redirect_uri: redirectUri,
      scope: provider.scope,
    }),
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
    },
  };

  return requestJson<OAuthTokenResponse>(provider.request_token_url, options);
};

const getTokenExpiry = (data: OAuthTokenResponse) =>
  (data.expires_in ?? 0) > 0
    ? (Date.now() + data.expires_in * 1000).toString()
    : undefined;

export const makeSsoAuthState = (
  provider: SsoIdentityProvider,
  data: OAuthTokenResponse,
): EnterpriseAuthState | null => {
  // prefer jwt id_token
  const token = data.id_token || data.access_token;
  if (!token) {
    return null;
  }

  return {
    type: 'sso',
    clientId: provider.client_id,
    idToken: token,
    accessToken: data.access_token ?? '',
    refreshToken: data.refresh_token,
    expiry: getTokenExpiry(data),
  };
};

export const makeHasuraSsoAuthState = (
  data: OAuthTokenResponse,
  envVars: EnvVars,
): EnterpriseAuthState | null => {
  // prefer jwt id_token
  const token = data.id_token || data.access_token;
  if (!token) {
    return null;
  }

  let decodedToken: HasuraJWTPayload;
  try {
    decodedToken = decodeToken(token) || {};
  } catch {
    return null;
  }

  const project = decodedToken?.project;
  return {
    type: 'hasura-sso',
    idToken: token,
    accessToken: data.access_token ?? '',
    refreshToken: data.refresh_token,
    expiry: getTokenExpiry(data),
    userId: decodedToken.sub,
    project: {
      id: project?.['id'] ?? envVars.projectID,
      name: project?.['name'] ?? envVars.projectName,
      privileges: (decodedToken.collaborator_privileges as Privilege[]) ?? [],
      metricsFQDN: decodedToken.metrics_fqdn,
      plan_name: project?.['plan_name'],
      entitlements: project?.['entitlements'],
    },
  };
};

// refresh the access token a bit before it actually expires
// so in-flight requests don't race the expiry
const TOKEN_REFRESH_LEEWAY_MS = 60 * 1000;

export const shouldRefreshAuthState = (state: EnterpriseAuthState | null) => {
  if (state?.type !== 'sso' && state?.type !== 'hasura-sso') {
    return false;
  }

  const expiry = Number(state.expiry);
  if (!state.expiry || Number.isNaN(expiry)) {
    return false;
  }

  return Date.now() >= expiry - TOKEN_REFRESH_LEEWAY_MS;
};

export const buildEnterpriseAuthHeaders = (
  state: EnterpriseAuthState | null,
): Record<string, string> => {
  switch (state?.type) {
    case 'admin-secret':
      return {
        ...CONSTANT_HEADERS,
        [ADMIN_SECRET_HEADER_KEY]: state.adminSecret,
      };
    case 'pat':
      return {
        ...CONSTANT_HEADERS,
        [HASURA_COLLABORATOR_TOKEN]: `pat ${state.pat}`,
      };
    case 'hasura-sso':
      return {
        ...CONSTANT_HEADERS,
        [HASURA_COLLABORATOR_TOKEN]: `IDToken ${state.idToken}`,
      };
    case 'sso':
      return {
        ...CONSTANT_HEADERS,
        [HASURA_SSO_TOKEN]: `IDToken ${state.idToken}`,
      };
    default:
      return CONSTANT_HEADERS;
  }
};

export const buildMetricsAuthHeaders = (
  state: EnterpriseAuthState | null,
): Record<string, string> => {
  switch (state?.type) {
    case 'pat':
      return {
        ...CONSTANT_HEADERS,
        Authorization: `pat ${state.pat}`,
      };
    case 'hasura-sso':
    case 'sso':
      return {
        ...CONSTANT_HEADERS,
        Authorization: `Bearer ${state.accessToken}`,
      };
    default:
      return CONSTANT_HEADERS;
  }
};

export type HasuraJWTPayload = JwtPayload & {
  allowed_schemas?: string[];
  allowed_tables?: Record<string, any>;
  collaborator_privileges?: string[];
  project?: Record<string, any>;
  metrics_fqdn?: string;
};

export const decodeToken = (idToken: string) => {
  const decoded = jwtDecode<HasuraJWTPayload>(idToken);
  return decoded;
};
