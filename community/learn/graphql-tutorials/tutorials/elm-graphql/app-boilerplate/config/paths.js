'use strict';

const path = require('path');
const fs = require('fs');
const url = require('url');
const cosmiconfig = require('cosmiconfig');

// Make sure any symlinks in the project folder are resolved:
// https://github.com/facebookincubator/create-react-app/issues/637
const appDirectory = fs.realpathSync(process.cwd());
const resolveApp = relativePath => path.resolve(appDirectory, relativePath);

// We look for configration in files supported by cosmiconfig by default:
// https://github.com/davidtheclark/cosmiconfig
const explorer = cosmiconfig('elmapp');
const result = explorer.searchSync(appDirectory);
const config = result ? result.config : loadElmJson();
const id = x => x;
const configureWebpack =
  typeof config.configureWebpack === 'function' ? config.configureWebpack : id;

// WARNING:
// We support config in elm.json only for legacy reasons.
// elm-package removes the settings, so this will be removed in the future.
function loadElmJson() {
  try {
    const elmJson = require(resolveApp('elm.json'));
    if (elmJson.homepage || elmJson.proxy) {
      return elmJson;
    }
  } catch (error) {
    return {};
  }
  return {};
}

const envPublicUrl = process.env.PUBLIC_URL;

function ensureSlash(path, needsSlash) {
  const hasSlash = path.endsWith('/');
  if (hasSlash && !needsSlash) {
    return path.substr(path, path.length - 1);
  } else if (!hasSlash && needsSlash) {
    return `${path}/`;
  }
  return path;
}

const getPublicUrl = appConfig => {
  if (envPublicUrl) {
    return envPublicUrl;
  }
  return appConfig.homepage;
};

// We use `PUBLIC_URL` environment variable or "homepage" field to infer
// "public path" at which the app is served.
// Webpack needs to know it to put the right <script> hrefs into HTML even in
// single-page apps that may serve index.html for nested URLs like /todos/42.
// We can't use a relative path in HTML because we don't want to load something
// like /todos/42/static/js/bundle.7289d.js. We have to know the root.
function getServedPath(appConfig) {
  const publicUrl = getPublicUrl(appConfig);
  const servedUrl =
    envPublicUrl || (publicUrl ? url.parse(publicUrl).pathname : '/');
  return ensureSlash(servedUrl, true);
}

module.exports = {
  appPath: resolveApp('.'),
  appPublic: resolveApp('./public'),
  appHtml: resolveApp('./public/index.html'),
  appIndexJs: resolveApp('./src/index.js'),
  appSrc: resolveApp('./src'),
  dotenv: resolveApp('./.env'),
  entry: resolveApp('./src/index.js'),
  appBuild: resolveApp('./build'),
  elmJson: resolveApp('./elm.json'),
  elm: require.resolve('elm/bin/elm'),
  publicUrl: getPublicUrl(config),
  servedPath: getServedPath(config),
  proxy: config.proxy,
  setupProxy: config.setupProxy,
  configureWebpack
};
