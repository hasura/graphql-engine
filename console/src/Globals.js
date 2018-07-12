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
  nodeEnv: window.__env.nodeEnv,
  devDataApiUrl: window.__env.devDataApiUrl,
  accessKey: window.__env.accessKey,
  consoleMode: window.__env.consoleMode,
  urlPrefix: checkExtraSlashes(window.__env.urlPrefix),
  analyticsUrl: 'wss://analytics.stats.hasura.io/v1/ws',
  isAnalyticsEnabled: window.__env.isAnalyticsEnabled,
  projectVersion: window.__env.projectVersion,
};

// set defaults
if (!window.__env.urlPrefix) {
  globals.urlPrefix = '/';
}

if (!window.__env.consoleMode) {
  globals.consoleMode = 'hasuradb';
}

if (!window.__env.accessKey) {
  globals.accessKey = null;
}

if (!window.__env.isAnalyticsEnabled) {
  globals.isAnalyticsEnabled = true;
}

if (globals.consoleMode === 'hasuradb') {
  const windowUrl = window.location.protocol + '//' + window.location.host;
  globals.dataApiUrl = windowUrl;
  if (globals.nodeEnv === 'development') {
    globals.dataApiUrl = globals.devDataApiUrl;
  }
}

export default globals;
