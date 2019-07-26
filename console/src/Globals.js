import { SERVER_CONSOLE_MODE } from './constants';

/* helper tools to format explain json */

/* eslint-disable */
import sqlFormatter from './helpers/sql-formatter.min';
import hljs from './helpers/highlight.min';
import { getFeaturesCompatibility } from './helpers/versionUtils';
/* eslint-enable */

/* */

const checkExtraSlashes = url => {
  if (!url) {
    return url;
  }
  if (url[url.length - 1] === '/') {
    return url.slice(0, url.length - 1);
  }
  return url;
};

const globals = {
  apiHost: window.__env.apiHost,
  apiPort: window.__env.apiPort,
  dataApiUrl: checkExtraSlashes(window.__env.dataApiUrl),
  devDataApiUrl: window.__env.devDataApiUrl,
  nodeEnv: window.__env.nodeEnv,
  adminSecret: window.__env.adminSecret || window.__env.accessKey,
  isAdminSecretSet:
    window.__env.isAdminSecretSet || window.__env.isAccessKeySet,
  adminSecretLabel:
    window.__env.isAdminSecretSet !== undefined ||
    window.__env.adminSecret !== undefined
      ? 'admin-secret'
      : 'access-key',
  consoleMode:
    window.__env.consoleMode === 'hasuradb'
      ? 'server'
      : window.__env.consoleMode,
  urlPrefix: checkExtraSlashes(window.__env.urlPrefix),
  enableTelemetry: window.__env.enableTelemetry,
  telemetryTopic:
    window.__env.nodeEnv !== 'development' ? 'console' : 'console_test',
  assetsPath: window.__env.assetsPath,
  serverVersion: window.__env.serverVersion,
  featuresCompatibility: window.__env.serverVersion
    ? getFeaturesCompatibility(window.__env.serverVersion)
    : null,
};

// set defaults
if (!window.__env.urlPrefix) {
  globals.urlPrefix = '/';
}

if (!window.__env.consoleMode) {
  globals.consoleMode = SERVER_CONSOLE_MODE;
}
if (!globals.adminSecret) {
  globals.adminSecret = null;
}

if (globals.isAdminSecretSet === undefined) {
  globals.isAdminSecretSet = false;
}

if (
  window &&
  typeof window === 'object' &&
  !window.sqlFormatter &&
  !window.hljs
) {
  window.sqlFormatter = sqlFormatter;
  window.hljs = hljs;
}

if (globals.consoleMode === SERVER_CONSOLE_MODE) {
  if (globals.nodeEnv !== 'development') {
    if (window.__env.consolePath) {
      const safeCurrentUrl = checkExtraSlashes(window.location.href);
      globals.dataApiUrl = safeCurrentUrl.slice(
        0,
        safeCurrentUrl.lastIndexOf(window.__env.consolePath)
      );
      const currentPath = checkExtraSlashes(window.location.pathname);
      globals.urlPrefix =
        currentPath.slice(
          0,
          currentPath.lastIndexOf(window.__env.consolePath)
        ) + '/console';
    } else {
      const windowUrl = window.location.protocol + '//' + window.location.host;
      globals.dataApiUrl = windowUrl;
    }
  }
  /*
   * Require the exact usecase
  if (globals.nodeEnv === 'development') {
    globals.dataApiUrl = globals.devDataApiUrl;
  }
  */
}
export default globals;
