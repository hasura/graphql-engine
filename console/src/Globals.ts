/* eslint no-underscore-dangle: 0 */
import { SERVER_CONSOLE_MODE } from './constants';
import { getFeaturesCompatibility } from './helpers/versionUtils';
import { stripTrailingSlash } from './components/Common/utils/urlUtils';
import { isEmpty } from './components/Common/utils/jsUtils';
import { Nullable } from './components/Common/utils/tsUtils';

type ConsoleType = 'oss' | 'cloud' | 'pro' | 'pro-cloud';

declare global {
  interface Window {
    __env: {
      nodeEnv: string;
      apiHost: string;
      apiPort: string;
      dataApiUrl: string;
      urlPrefix: string;
      adminSecret: string;
      isAdminSecretSet: boolean;
      consoleMode: string;
      enableTelemetry: boolean;
      assetsPath: string;
      serverVersion: string;
      consolePath: string;
      cliUUID: string;
      consoleId: Nullable<string>;
      herokuOAuthClientId: string;
      tenantID: Nullable<string>;
      projectID: Nullable<string>;
      userRole: Nullable<string>;
      cloudRootDomain: Nullable<string>;
      luxDataHost: Nullable<string>;
      consoleType: ConsoleType;
      eeMode: Nullable<string>;
    };
  }
  const CONSOLE_ASSET_VERSION: string;
}

/* initialize globals */

const isProduction = window.__env?.nodeEnv !== 'development';

const globals = {
  apiHost: window.__env?.apiHost,
  apiPort: window.__env?.apiPort,
  dataApiUrl: stripTrailingSlash(window.__env?.dataApiUrl), // overridden below if server mode
  urlPrefix: stripTrailingSlash(window.__env?.urlPrefix || '/'), // overridden below if server mode in production
  adminSecret: window.__env?.adminSecret || null, // gets updated after login/logout in server mode
  isAdminSecretSet:
    window.__env?.isAdminSecretSet ||
    !isEmpty(window.__env?.adminSecret) ||
    false,
  consoleMode: window.__env?.consoleMode || SERVER_CONSOLE_MODE,
  enableTelemetry: window.__env?.enableTelemetry,
  telemetryTopic: isProduction ? 'console' : 'console_test',
  assetsPath: window.__env?.assetsPath,
  serverVersion: window.__env?.serverVersion,
  consoleAssetVersion: CONSOLE_ASSET_VERSION, // set during console build
  featuresCompatibility: window.__env?.serverVersion
    ? getFeaturesCompatibility(window.__env?.serverVersion)
    : null,
  cliUUID: window.__env?.cliUUID,
  hasuraUUID: '',
  telemetryNotificationShown: false,
  isProduction,
  herokuOAuthClientId: window.__env?.herokuOAuthClientId,
  hasuraCloudTenantId: window.__env?.tenantID,
  hasuraCloudProjectId: window.__env?.projectID,
  cloudDataApiUrl: `${window.location?.protocol}//data.${window.__env?.cloudRootDomain}`,
  luxDataHost: window.__env?.luxDataHost,
  userRole: undefined, // userRole is not applicable for the OSS console
  consoleType: window.__env?.consoleType,
  eeMode: window.__env?.eeMode === 'true',
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
        currentUrl = stripTrailingSlash(window.__env?.dataApiUrl);
        slicePath = false;
      }
      const currentPath = stripTrailingSlash(window.location?.pathname);

      // NOTE: perform the slice if not on team console
      // as on team console, we're using the server
      // endpoint directly to load the assets of the console
      if (slicePath) {
        globals.dataApiUrl = currentUrl.slice(
          0,
          currentUrl.lastIndexOf(consolePath)
        );
      }

      globals.urlPrefix = `${currentPath.slice(
        0,
        currentPath.lastIndexOf(consolePath)
      )}/console`;
    } else {
      const windowHostUrl = `${window.location?.protocol}//${window.location?.host}`;
      globals.dataApiUrl = windowHostUrl;
    }
  }
}

export default globals;
