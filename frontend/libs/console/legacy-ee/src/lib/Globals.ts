import { type SsoIdentityProviders } from './types';
import { globals } from '@hasura/console-legacy-ce';
import { isEmpty } from './utils/validation';

type CeConsoleEnvVars = typeof window.__env;

type EeConsoleEnvVars = CeConsoleEnvVars & {
  readonly hasuraMetricsUrl?: string;
  readonly hasuraOAuthUrl?: string;
  readonly hasuraOAuthScopes?: string;
  readonly isPATSet?: boolean;
  readonly personalAccessToken?: string;
  readonly projectId?: string;
  readonly versionedAssetsPath?: string;
  readonly pro?: boolean;
  readonly isMetadataAPIEnabled?: boolean;
  readonly ssoEnabled?: string;

  // Available only in EE
  readonly ssoIdentityProviders?: SsoIdentityProviders;
};

const stripTrailingSlash = (url: string) => url.replace(/\/$/, '');

const windowEnv: EeConsoleEnvVars = window.__env;

const getHasuraMetricsUrl = () =>
  windowEnv.hasuraMetricsUrl ||
  (windowEnv.hasuraOAuthUrl || '').replace('oauth', 'metrics') ||
  'http://metrics.lux-dev.hasura.me';

const extendedGlobals = {
  ...globals,
  adminSecretLabel: 'admin-secret',
  collabLabel: 'hasura-collaborator-token',
  ssoLabel: 'hasura-sso-token',
  patLabel: 'Hasura-Collaborator-Token',
  hasuraClientID: windowEnv.consoleId,
  metricsApiUrl: stripTrailingSlash(getHasuraMetricsUrl()),
  hasuraOAuthUrl: stripTrailingSlash(
    windowEnv.hasuraOAuthUrl || 'http://oauth.lux-dev.hasura.me'
  ),
  hasuraOAuthScopes: windowEnv.hasuraOAuthScopes || 'openid offline',
  relativeOAuthRedirectUrl: '/oauth2/callback',
  relativeOAuthTokenUrl: '/oauth2/token',
  isPATSet: windowEnv.isPATSet || false,
  personalAccessToken: windowEnv.personalAccessToken || null,
  projectName: windowEnv.projectName,
  projectId: windowEnv?.projectId,
  versionedAssetsPath:
    windowEnv.versionedAssetsPath ||
    `${globals.assetsPath}/channel/versioned/${globals.serverVersion}`,
  pro: windowEnv.pro === true,
  adminSecret: windowEnv.adminSecret,
  isMetadataAPIEnabled: windowEnv.isMetadataAPIEnabled,
  userRole: windowEnv.userRole,
  isAdminSecretSet:
    windowEnv?.isAdminSecretSet || !isEmpty(windowEnv?.adminSecret) || false,
  consoleType: windowEnv.consoleType,
  ssoEnabled: windowEnv.ssoEnabled === 'true',

  ssoIdentityProviders: windowEnv.ssoIdentityProviders || [],
};

export default extendedGlobals;
