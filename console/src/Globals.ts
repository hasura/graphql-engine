/* eslint no-underscore-dangle: 0 */
import { SERVER_CONSOLE_MODE } from './constants';
import { getFeaturesCompatibility } from './helpers/versionUtils';
import { stripTrailingSlash } from './components/Common/utils/urlUtils';
import { isEmpty } from './components/Common/utils/jsUtils';

// TODO: move this section to a more appropriate location
/* set helper tools into window */
import sqlFormatter from './helpers/sql-formatter.min';

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
    sqlFormatter: unknown;
    hljs: unknown;
  }
  const CONSOLE_ASSET_VERSION: string;
}

if (
  (window as Window) &&
  typeof window === 'object' &&
  !window.sqlFormatter &&
  !window.hljs
) {
  window.sqlFormatter = sqlFormatter;
  window.hljs = hljs;
}

/* add tracking script */
const head = document.getElementsByTagName('head')[0];
const scriptTag = document.createElement('script');
const scriptContent = `
!function(t,e){var o,n,p,r;e.__SV||(window.posthog=e,e._i=[],e.init=function(i,s,a){function g(t,e){var o=e.split(".");2==o.length&&(t=t[o[0]],e=o[1]),t[e]=function(){t.push([e].concat(Array.prototype.slice.call(arguments,0)))}}(p=t.createElement("script")).type="text/javascript",p.async=!0,p.src=s.api_host+"/static/array.js",(r=t.getElementsByTagName("script")[0]).parentNode.insertBefore(p,r);var u=e;for(void 0!==a?u=e[a]=[]:a="posthog",u.people=u.people||[],u.toString=function(t){var e="posthog";return"posthog"!==a&&(e+="."+a),t||(e+=" (stub)"),e},u.people.toString=function(){return u.toString(1)+".people (stub)"},o="capture identify alias people.set people.set_once set_config register register_once unregister opt_out_capturing has_opted_out_capturing opt_in_capturing reset isFeatureEnabled onFeatureFlags".split(" "),n=0;n<o.length;n++)g(u,o[n]);e._i.push([i,s,a])},e.__SV=1)}(document,window.posthog||[]);
  posthog.init('FiK8uKNJInlE9XcIDMD3VasLUXAcd5xy__qE0qCmpbs', {api_host: 'https://hasura-posthog-test.herokuapp.com'})
`;
const inlineScript = document.createTextNode(scriptContent);
scriptTag.appendChild(inlineScript);
head.appendChild(scriptTag);

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
