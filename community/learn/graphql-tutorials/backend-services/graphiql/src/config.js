/* @flow */
const appConfig = require('../appconfig');

const host = appConfig.appHost;
const port = appConfig.port[process.env.NODE_ENV || 'development'];

// require('babel-polyfill');

const environment = {
  development: {
    isProduction: false,
  },
  production: {
    isProduction: true,
  },
}[process.env.NODE_ENV || 'development'];

module.exports = Object.assign(
  {
    host: host,
    port: port,
  },
  environment
);
