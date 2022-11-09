/* eslint no-underscore-dangle: 0 */
import { getFeaturesCompatibility } from './helpers/versionUtils';

import { sentry } from './features/TracingTools/sentry';
import { isEmpty } from './components/Common/utils/jsUtils';
import { stripTrailingSlash } from './components/Common/utils/urlUtils';

import { SERVER_CONSOLE_MODE } from './constants';
import { parseConsoleType, ConsoleType } from './utils/envUtils';

export type LuxFeature =
  | 'DatadogIntegration'
  | 'ProUser'
  | 'CloudUser'
  | 'V1V2Migration'
  | 'GithubIntegration'
  | 'CloudDedicatedVPC'
  | 'GCPSupport'
  | 'Avalara'
  | 'NeonDatabaseIntegration'
  | string;

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
  consoleSentryDsn?: string; // Corresponds to the HASURA_CONSOLE_SENTRY_DSN environment variable
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
  consoleSentryDsn?: string; // Corresponds to the HASURA_CONSOLE_SENTRY_DSN environment variable
};

type ProLiteServerEnv = {
  consoleType: 'pro-lite';
  consoleId: string;
  consoleMode: 'server';
  assetsPath: string;
  consolePath: string;
  enableTelemetry: boolean;
  isAdminSecretSet: boolean;
  serverVersion: string;
  urlPrefix: string;
  consoleSentryDsn?: string; // Corresponds to the HASURA_CONSOLE_SENTRY_DSN environment variable
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
  neonOAuthClientId?: string;
  neonRootDomain?: string;
  allowedLuxFeatures?: LuxFeature[];
  userId?: string;
  consoleSentryDsn?: string; // Corresponds to the HASURA_CONSOLE_SENTRY_DSN environment variable
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
  consoleSentryDsn?: string; // Corresponds to the HASURA_CONSOLE_SENTRY_DSN environment variable
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
  consoleSentryDsn?: string; // Corresponds to the HASURA_CONSOLE_SENTRY_DSN environment variable
};

type ProCliEnv = CloudCliEnv;
type ProLiteCliEnv = CloudCliEnv;

// Until this non-discriminated-union-based `EnvVars` exist, please keep the following spreadsheet
// https://docs.google.com/spreadsheets/d/10feBESWKCfFuh7g9436Orp4i4fNoQxjnt5xxhrrdtJo/edit#gid=0
// updated with all the env vars that the Console receives and their possible values. The spreadsheet acts as the source of truth for the environment variables, at the moment
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
  userRole?: string;
  neonOAuthClientId?: string;
  neonRootDomain?: string;
  allowedLuxFeatures?: LuxFeature[];
  userId?: string;
  cdnAssets?: boolean;
  consoleSentryDsn?: string; // Corresponds to the HASURA_CONSOLE_SENTRY_DSN environment variable
} & (
  | OSSServerEnv
  | CloudServerEnv
  | ProServerEnv
  | ProLiteServerEnv
  | OSSCliEnv
  | CloudCliEnv
  | ProCliEnv
  | ProLiteCliEnv
);

declare global {
  interface Window {
    __env: EnvVars;
    /**
     * Consuming Heap is allowed only through the TracingTools/heap module, never directly.
     * @deprecated (when marked as deprecated, the IDE shows it as strikethrough'ed, helping the
     * developers realize that they should not use it)
     */
    heap?: {
      addUserProperties: (properties: Record<string, string>) => void;
    };
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
  consoleSentryDsn: sentry.parseSentryDsn(window.__env?.consoleSentryDsn),
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
  neonOAuthClientId: window.__env?.neonOAuthClientId,
  neonRootDomain: window.__env?.neonRootDomain,
  allowedLuxFeatures: window.__env?.allowedLuxFeatures || [],
  cloudDataApiUrl: `${window.location?.protocol}//data.${window.__env?.cloudRootDomain}`,
  luxDataHost: window.__env?.luxDataHost,
  userRole: window.__env?.userRole || undefined,
  userId: window.__env?.userId || undefined,
  consoleType: window.__env?.consoleType // FIXME : this check can be removed when the all CLI environments are set with the console type, some CLI environments could have empty consoleType
    ? parseConsoleType(window.__env?.consoleType)
    : ('' as ConsoleType),
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
