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
  accessKey: window.__env.accessKey,
};
export default globals;
