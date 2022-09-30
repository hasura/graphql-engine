import { globals } from '@hasura/console-oss';

const stripTrailingSlash = url => url.replace(/\/$/, '');

const getHasuraMetricsUrl = () =>
  window.__env.hasuraMetricsUrl ||
  (window.__env.hasuraOAuthUrl || '').replace('oauth', 'metrics') ||
  'http://metrics.lux-dev.hasura.me';

const extendedGlobals = {
  ...globals,
  adminSecretLabel: 'admin-secret',
  collabLabel: 'hasura-collaborator-token',
  patLabel: 'Hasura-Collaborator-Token',
  hasuraClientID: window.__env.consoleId,
  metricsApiUrl: stripTrailingSlash(getHasuraMetricsUrl()),
  hasuraOAuthUrl: stripTrailingSlash(
    window.__env.hasuraOAuthUrl || 'http://oauth.lux-dev.hasura.me'
  ),
  hasuraOAuthScopes: window.__env.hasuraOAuthScopes || 'openid offline',
  relativeOAuthRedirectUrl: '/oauth2/callback',
  relativeOAuthTokenUrl: '/oauth2/token',
  isPATSet: window.__env.isPATSet || false,
  personalAccessToken: window.__env.personalAccessToken || null,
  projectName: window.__env.projectName,
  projectId: window.__env?.projectId,
  versionedAssetsPath:
    window.__env.versionedAssetsPath ||
    `${globals.assetsPath}/channel/versioned/${globals.serverVersion}`,
  pro: window.__env.pro === true,
  adminSecret: window.__env.adminSecret,
  isMetadataAPIEnabled: window.__env.isMetadataAPIEnabled,
  userRole: window.__env.userRole,
  isAdminSecretSet: window.__env.isAdminSecretSet,
  consoleType: window.__env.consoleType,
};

export default extendedGlobals;
