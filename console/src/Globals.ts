/* eslint no-underscore-dangle: 0 */
import { SERVER_CONSOLE_MODE } from './constants';
import { getFeaturesCompatibility } from './helpers/versionUtils';
import { stripTrailingSlash } from './components/Common/utils/urlUtils';
import { isEmpty } from './components/Common/utils/jsUtils';

type ConsoleType = 'oss' | 'cloud' | 'pro';

type UUID = string;

type OSSServerEnv = {
  consoleMode: 'server';
  consoleType: 'oss';
  assetsPath: string; // e.g. "https://graphql-engine-cdn.hasura.io/console/assets"
  consolePath: string; // e.g. "/console"
  enableTelemetry: boolean;
  isAdminSecretSet: boolean;
  serverVersion: string; // e.g. "v2.7.0"
  urlPrefix: string; // e.g. "/console"
  cdnAssets: boolean;
};

type ProServerEnv = {
  consoleType: 'pro';
  consoleId: string;
  consoleMode: 'server';
  assetsPath: string;
  consolePath: string;
  enableTelemetry: boolean;
  isAdminSecretSet: boolean;
  serverVersion: string;
  urlPrefix: string;
};

type CloudUserRole = 'owner' | 'user';

type CloudServerEnv = {
  consoleMode: 'server';
  consoleType: 'cloud';
  adminSecret: string;
  assetsPath: string;
  cloudRootDomain: string; // e.g. "pro.hasura.io"
  consoleId: string; // e.g. "40d778e7-1324-4500-bf69-5f9e58f70803_console"
  consolePath: string;
  dataApiUrl: string; // e.g. "https://rich-jackass-37.hasura.app"
  eeMode: string;
  herokuOAuthClientId: UUID;
  isAdminSecretSet: boolean;
  luxDataHost: string; // e.g. "data.pro.hasura.io"
  projectID: UUID;
  serverVersion: string;
  tenantID: UUID;
  urlPrefix: string;
  userRole: CloudUserRole;
};

type OSSCliEnv = {
  consoleMode: 'cli';
  adminSecret: string;
  apiHost: string; // e.g. "http://localhost"
  apiPort: string; // e.g. "9693"
  assetsPath: string;
  cliUUID: UUID;
  consolePath: string;
  dataApiUrl: string;
  enableTelemetry: boolean;
  serverVersion: string;
  urlPrefix: string;
};

export type CloudCliEnv = {
  consoleMode: 'cli';
  adminSecret: string;
  apiHost: string;
  apiPort: string;
  assetsPath: string;
  cliUUID: string;
  consolePath: string;
  dataApiUrl: string;
  enableTelemetry: boolean;
  serverVersion: string;
  urlPrefix: string;
  /* NOTE
     While in CLI mode we are relying on the "pro" key to determine if we are in the pro console or not.
     We could ask the CLI team to add a consoleType env var so that we can rely on values "cloud" | "pro",
     like in the server console mode
  */
  pro: true;
  projectId: UUID;
  isAdminSecretSet: boolean;
};

type ProCliEnv = CloudCliEnv;

export type EnvVars = {
  nodeEnv?: string;
  apiHost?: string;
  apiPort?: string;
  dataApiUrl?: string;
  adminSecret?: string;
  serverVersion: string;
  cliUUID?: string;
  tenantID?: UUID;
  projectID?: UUID;
  cloudRootDomain?: string;
  herokuOAuthClientId?: string;
  luxDataHost?: string;
  isAdminSecretSet?: boolean;
  enableTelemetry?: boolean;
  consoleType?: ConsoleType;
  eeMode?: string;
  consoleId?: string;
} & (
  | OSSServerEnv
  | CloudServerEnv
  | ProServerEnv
  | OSSCliEnv
  | CloudCliEnv
  | ProCliEnv
);

declare global {
  interface Window {
    __env: EnvVars;
  }
  const CONSOLE_ASSET_VERSION: string;
}

/* initialize globals */

const isProduction = window.__env?.nodeEnv !== 'development';

const globals = {
  apiHost: window.__env?.apiHost,
  apiPort: window.__env?.apiPort,
  dataApiUrl: stripTrailingSlash(window.__env?.dataApiUrl || ''), // overridden below if server mode
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
  serverVersion: window.__env?.serverVersion || '',
  consoleAssetVersion: CONSOLE_ASSET_VERSION, // set during console build
  featuresCompatibility: window.__env?.serverVersion
    ? getFeaturesCompatibility(window.__env?.serverVersion || '')
    : null,
  cliUUID: window.__env?.cliUUID || '',
  hasuraUUID: '',
  telemetryNotificationShown: false,
  isProduction,
  herokuOAuthClientId: window.__env?.herokuOAuthClientId,
  hasuraCloudTenantId: window.__env?.tenantID,
  hasuraCloudProjectId: window.__env?.projectID,
  cloudDataApiUrl: `${window.location?.protocol}//data.${window.__env?.cloudRootDomain}`,
  luxDataHost: window.__env?.luxDataHost,
  userRole: undefined, // userRole is not applicable for the OSS console
  consoleType: window.__env?.consoleType || '',
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
        currentUrl = stripTrailingSlash(window.__env?.dataApiUrl || '');
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
