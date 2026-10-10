import {
  CLIENT_NAME_HEADER_VALUE,
  CONTENT_TYPE_HEADER,
  HASURA_CLIENT_NAME,
} from '@hasura/shared/types';

export const CONSTANT_HEADERS = {
  [CONTENT_TYPE_HEADER]: 'application/json',
  [HASURA_CLIENT_NAME]: CLIENT_NAME_HEADER_VALUE,
};

export const RELATIVE_OAUTH_REDIRECT_URL = '/oauth2/callback';
export const RELATIVE_OAUTH_TOKEN_URL = '/oauth2/token';
export const HASURA_OAUTH_SCOPES = 'openid offline';
