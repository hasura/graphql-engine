import { globals } from '@hasura/console-legacy-ce';
import { isEmpty } from './utils/validation';
import { EnvVars, SsoIdentityProviders } from '@hasura/shared/types';

type CeConsoleEnvVars = typeof window.__env;

type EeConsoleEnvVars = CeConsoleEnvVars & {
  readonly hasuraMetricsUrl?: string;
  readonly hasuraOAuthUrl?: string;
  readonly isPATSet?: boolean;
  readonly personalAccessToken?: string;
  readonly projectId?: string;
  readonly versionedAssetsPath?: string;
  readonly pro?: boolean;
  readonly isMetadataAPIEnabled?: boolean;
  readonly isAdminSecretDisabled?: boolean;
  readonly ssoEnabled?: string;

  // Available only in EE
  readonly ssoIdentityProviders?: SsoIdentityProviders;
};

const stripTrailingSlash = (url: string) => url.replace(/\/$/, '');

const windowEnv: EeConsoleEnvVars = window.__env;

export const getHasuraMetricsUrl = (envVars: EnvVars): string => {
  if (
    envVars.consoleMode !== 'server' ||
    !envVars.consoleType ||
    envVars.consoleType === 'oss' ||
    envVars.consoleType === 'cloud' ||
    !envVars.projectID
  ) {
    return '';
  }

  const metricsUrl =
    envVars.hasuraMetricsUrl ||
    ('hasuraOAuthUrl' in envVars && envVars.hasuraOAuthUrl
      ? envVars.hasuraOAuthUrl
      : ''
    ).replace('oauth', 'metrics') ||
    'http://metrics.lux-dev.hasura.me';

  return stripTrailingSlash(metricsUrl);
};

const extendedGlobals = {
  ...globals,
  hasuraClientID: windowEnv.consoleId,
  metricsApiUrl: stripTrailingSlash(getHasuraMetricsUrl(windowEnv)),
  hasuraOAuthUrl: stripTrailingSlash(
    windowEnv.hasuraOAuthUrl || 'http://oauth.lux-dev.hasura.me',
  ),
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
  isAdminSecretDisabled: windowEnv.isAdminSecretDisabled || false,
  consoleType: windowEnv.consoleType,
  ssoEnabled: windowEnv.ssoEnabled === 'true',

  ssoIdentityProviders: windowEnv.ssoIdentityProviders || [],
};

export default extendedGlobals;
