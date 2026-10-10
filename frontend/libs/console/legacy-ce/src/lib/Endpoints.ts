import consoleGlobals from './Globals';
import { getEndpoints } from '@hasura/shared/context';

export const baseUrl = consoleGlobals.dataApiUrl;

export const globalCookiePolicy: RequestCredentials = 'same-origin';

const endpoints = getEndpoints(window.__env, baseUrl);

export default endpoints;
