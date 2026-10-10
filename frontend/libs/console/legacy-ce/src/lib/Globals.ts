import { GlobalWindowHeap } from '@hasura/shared/analytics';
import { EnvVars, SERVER_CONSOLE_MODE } from '@hasura/shared/types';
import {
  parseConsoleType,
  getFeaturesCompatibility,
  isEmpty,
  stripTrailingSlash,
} from '@hasura/shared/utils';

declare global {
  interface Window extends GlobalWindowHeap {
    __env: EnvVars;
  }
  const CONSOLE_ASSET_VERSION: string;
}

/* initialize globals */

const isProduction = window.__env?.nodeEnv !== 'development';
const globals = {
  apiHost: window.__env?.apiHost,
  apiPort: window.__env?.apiPort,
  dataApiUrl: stripTrailingSlash(window.__env?.dataApiUrl || ''), // overridden below if server mode
  urlPrefix: stripTrailingSlash(window.__env?.urlPrefix || '/'), // overridden below if server mode in production
  adminSecret: window.__env?.adminSecret || null, // gets updated after login/logout in server mode
  isAdminSecretSet:
    window.__env?.isAdminSecretSet ||
    !isEmpty(window.__env?.adminSecret) ||
    false,
  isAdminSecretDisabled: window.__env?.isAdminSecretDisabled || false,
  consoleMode: window.__env?.consoleMode || SERVER_CONSOLE_MODE,
  enableTelemetry: window.__env?.enableTelemetry,
  telemetryTopic: isProduction ? 'console-v2' : 'console-test-v2', // updated to v2 to ignore legacy redux based events from earlier console versions
  assetsPath: window.__env?.assetsPath,
  serverVersion: window.__env?.serverVersion || '',
  consoleAssetVersion: CONSOLE_ASSET_VERSION, // set during console build
  featuresCompatibility: window.__env?.serverVersion
    ? getFeaturesCompatibility(window.__env?.serverVersion || '')
    : null,
  cliUUID: window.__env?.cliUUID || '',
  hasuraUUID: '',
  isProduction,
  herokuOAuthClientId: window.__env?.herokuOAuthClientId,
  hasuraCloudTenantId: window.__env?.tenantID,
  hasuraCloudProjectId: window.__env?.projectID,
  hasuraCloudProjectName: window.__env?.projectName,
  neonOAuthClientId: window.__env?.neonOAuthClientId,
  neonRootDomain: window.__env?.neonRootDomain,
  slackOAuthClientId: window.__env?.slackOAuthClientId,
  slackRootDomain: window.__env?.slackRootDomain,
  allowedLuxFeatures: window.__env?.allowedLuxFeatures || [],
  luxDataHost: window.__env?.luxDataHost
    ? stripTrailingSlash(window.__env.luxDataHost)
    : // stripTrailingSlash is used to ensure correctness in Endpoints because we append /v1/graphql to luxDataHost in endpoints.
      undefined,
  schemaRegistryHost: window.__env?.schemaRegistryHost
    ? stripTrailingSlash(window.__env.schemaRegistryHost)
    : '',
  userRole: window.__env?.userRole || undefined,
  userId: window.__env?.userId || undefined,
  userEmail: window.__env?.userEmail || undefined,
  pricingPlan: window.__env?.pricingPlan || undefined,
  consoleType: window.__env?.consoleType // FIXME : this check can be removed when the all CLI environments are set with the console type, some CLI environments could have empty consoleType
    ? parseConsoleType(window.__env?.consoleType)
    : 'oss',
  eeMode: window.__env?.eeMode === 'true',
  adminSecretLabel: 'admin-secret',
};

if (globals.consoleMode === SERVER_CONSOLE_MODE) {
  if (!window.__env?.dataApiUrl) {
    globals.dataApiUrl = stripTrailingSlash(window.location?.href);
  }
  if (isProduction) {
    const consolePath = window.__env?.consolePath;
    if (consolePath) {
      let currentUrl = stripTrailingSlash(window.location?.href);
      let slicePath = true;
      if (window.__env?.dataApiUrl) {
        currentUrl = stripTrailingSlash(window.__env?.dataApiUrl || '');
        slicePath = false;
      }
      const currentPath = stripTrailingSlash(window.location?.pathname);

      // NOTE: perform the slice if not on team console
      // as on team console, we're using the server
      // endpoint directly to load the assets of the console
      if (slicePath) {
        globals.dataApiUrl = currentUrl.slice(
          0,
          currentUrl.lastIndexOf(consolePath),
        );
      }

      globals.urlPrefix = `${currentPath.slice(
        0,
        currentPath.lastIndexOf(consolePath),
      )}/console`;
    } else {
      const windowHostUrl = `${window.location?.protocol}//${window.location?.host}`;
      globals.dataApiUrl = windowHostUrl;
    }
  }
}

export type Globals = typeof globals;

export default globals;
