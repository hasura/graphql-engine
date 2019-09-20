import { SERVER_CONSOLE_MODE } from './constants';
import { getFeaturesCompatibility } from './helpers/versionUtils';
import { stripTrailingSlash } from './components/Common/utils/urlUtils';

/* set helper tools into window */

import sqlFormatter from './helpers/sql-formatter.min';
import hljs from './helpers/highlight.min';

if (
  window &&
  typeof window === 'object' &&
  !window.sqlFormatter &&
  !window.hljs
) {
  window.sqlFormatter = sqlFormatter;
  window.hljs = hljs;
}

/* initialize globals */

const globals = {
  apiHost: window.__env.apiHost,
  apiPort: window.__env.apiPort,
  dataApiUrl: stripTrailingSlash(window.__env.dataApiUrl),
  devDataApiUrl: window.__env.devDataApiUrl,
  nodeEnv: window.__env.nodeEnv,
  adminSecret: window.__env.adminSecret || null, // will be updated after login/logout
  isAdminSecretSet: window.__env.isAdminSecretSet || false,
  consoleMode: window.__env.consoleMode || SERVER_CONSOLE_MODE,
  urlPrefix: stripTrailingSlash(window.__env.urlPrefix) || '',
  enableTelemetry: window.__env.enableTelemetry,
  telemetryTopic:
    window.__env.nodeEnv !== 'development' ? 'console' : 'console_test',
  assetsPath: window.__env.assetsPath,
  serverVersion: window.__env.serverVersion,
  consoleAssetVersion: CONSOLE_ASSET_VERSION, // set during console build
  featuresCompatibility: window.__env.serverVersion
    ? getFeaturesCompatibility(window.__env.serverVersion)
    : null,
};

if (globals.consoleMode === SERVER_CONSOLE_MODE) {
  if (globals.nodeEnv !== 'development') {
    const consolePath = window.__env.consolePath;
    if (consolePath) {
      const currentUrl = stripTrailingSlash(window.location.href);
      globals.dataApiUrl = currentUrl.slice(
        0,
        currentUrl.lastIndexOf(consolePath)
      );

      const currentPath = stripTrailingSlash(window.location.pathname);
      globals.urlPrefix =
        currentPath.slice(0, currentPath.lastIndexOf(consolePath)) + '/console';
    } else {
      const windowUrl = window.location.protocol + '//' + window.location.host;

      globals.dataApiUrl = windowUrl;
    }
  }
}

export default globals;
