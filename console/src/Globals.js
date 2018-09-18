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
  accessKey: window.__env.accessKey,
  isAccessKeySet: window.__env.isAccessKeySet,
  consoleMode: window.__env.consoleMode,
  urlPrefix: checkExtraSlashes(window.__env.urlPrefix),
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

if (!window.__env.isAccessKeySet) {
  globals.isAccessKeySet = false;
}

if (globals.consoleMode === 'hasuradb') {
  if (globals.nodeEnv !== 'development') {
    const windowUrl = window.location.protocol + '//' + window.location.host;
    globals.dataApiUrl = windowUrl;
  }
  /*
   * Require the exact usecase
  if (globals.nodeEnv === 'development') {
    globals.dataApiUrl = globals.devDataApiUrl;
  }
  */
}

export default globals;
