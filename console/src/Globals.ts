/* eslint no-underscore-dangle: 0 */
import { SERVER_CONSOLE_MODE } from './constants';
import { getFeaturesCompatibility } from './helpers/versionUtils';
import { stripTrailingSlash } from './components/Common/utils/urlUtils';
import { isEmpty } from './components/Common/utils/jsUtils';

// TODO: move this section to a more appropriate location

const hljs = require('./helpers/highlight.min');

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
    };
    // TODO: Change
    hljs: unknown;
  }
  const CONSOLE_ASSET_VERSION: string;
}

if ((window as Window) && typeof window === 'object' && !window.hljs) {
  window.hljs = hljs;
}

/* initialize globals */

const isProduction = window.__env.nodeEnv !== 'development';

const globals = {
  apiHost: window.__env.apiHost,
  apiPort: window.__env.apiPort,
  dataApiUrl: stripTrailingSlash(window.__env.dataApiUrl), // overridden below if server mode
  urlPrefix: stripTrailingSlash(window.__env.urlPrefix || '/'), // overridden below if server mode in production
  adminSecret: window.__env.adminSecret || null, // gets updated after login/logout in server mode
  isAdminSecretSet:
    window.__env.isAdminSecretSet ||
    !isEmpty(window.__env.adminSecret) ||
    false,
  consoleMode: window.__env.consoleMode || SERVER_CONSOLE_MODE,
  enableTelemetry: window.__env.enableTelemetry,
  telemetryTopic: isProduction ? 'console' : 'console_test',
  assetsPath: window.__env.assetsPath,
  serverVersion: window.__env.serverVersion,
  consoleAssetVersion: CONSOLE_ASSET_VERSION, // set during console build
  featuresCompatibility: window.__env.serverVersion
    ? getFeaturesCompatibility(window.__env.serverVersion)
    : null,
  cliUUID: window.__env.cliUUID,
  hasuraUUID: '',
  telemetryNotificationShown: false,
  isProduction,
};
if (globals.consoleMode === SERVER_CONSOLE_MODE) {
  if (isProduction) {
    const consolePath = window.__env.consolePath;
    if (consolePath) {
      const currentUrl = stripTrailingSlash(window.location.href);
      const currentPath = stripTrailingSlash(window.location.pathname);

      globals.dataApiUrl = currentUrl.slice(
        0,
        currentUrl.lastIndexOf(consolePath)
      );

      globals.urlPrefix = `${currentPath.slice(
        0,
        currentPath.lastIndexOf(consolePath)
      )}/console`;
    } else {
      const windowHostUrl = `${window.location.protocol}//${window.location.host}`;

      globals.dataApiUrl = windowHostUrl;
    }
  }
}

export default globals;
