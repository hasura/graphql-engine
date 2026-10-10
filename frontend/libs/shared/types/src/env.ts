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

export type ConsoleType = 'oss' | 'cloud' | 'pro' | 'pro-lite';
export type ConsoleMode = 'cli' | 'server';

// SSO login identity configs that enable Single Sign-on
// integration with external OAuth providers for EE customer
export type SsoIdentityProvider = {
  client_id: string;
  name: string;
  scope: string;
  authorization_url: string;
  request_token_url: string;
};

export type SsoIdentityProviders = SsoIdentityProvider[];

export type OAuthTokenResponse = {
  access_token?: string;
  id_token?: string;
  expires_in: number;
  refresh_token?: string;
  token_type: string;
};

type UUID = string;

type OSSServerEnv = {
  consoleMode: 'server';
  consoleType: 'oss';
  assetsPath: string; // e.g. "https://graphql-engine-cdn.hasura.io/console/assets"
  consolePath: string; // e.g. "/console"
  enableTelemetry: boolean;
  isAdminSecretSet: boolean;
  isAdminSecretDisabled: boolean;
  serverVersion: string; // e.g. "v2.7.0"
  urlPrefix: string; // e.g. "/console"
  cdnAssets: boolean;
  consoleSentryDsn?: string; // Corresponds to the HASURA_CONSOLE_SENTRY_DSN environment variable
};

export type ProServerEnv = {
  consoleType: 'pro';
  consoleId: string;
  consoleMode: 'server';
  assetsPath: string;
  consolePath: string;
  enableTelemetry: boolean;
  isAdminSecretSet: boolean;
  isAdminSecretDisabled: boolean;
  serverVersion: string;
  urlPrefix: string;
  consoleSentryDsn?: string; // Corresponds to the HASURA_CONSOLE_SENTRY_DSN environment variable
  readonly ssoIdentityProviders?: SsoIdentityProviders;
};

type ProLiteServerEnv = {
  consoleType: 'pro-lite';
  consoleId: string;
  consoleMode: 'server';
  assetsPath: string;
  consolePath: string;
  enableTelemetry: boolean;
  isAdminSecretSet: boolean;
  isAdminSecretDisabled: boolean;
  hasuraMetricsUrl?: string;
  serverVersion: string;
  urlPrefix: string;
  consoleSentryDsn?: string; // Corresponds to the HASURA_CONSOLE_SENTRY_DSN environment variable
  ssoIdentityProviders?: SsoIdentityProviders;
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
  enableTelemetry: boolean;
  eeMode: string;
  herokuOAuthClientId: UUID;
  isAdminSecretSet: boolean;
  isAdminSecretDisabled: boolean;
  luxDataHost: string; // e.g. "data.pro.hasura.io"
  schemaRegistryHost: string;
  projectID: UUID;
  projectName: string;
  serverVersion: string;
  tenantID: UUID;
  urlPrefix: string;
  userRole: CloudUserRole;
  neonOAuthClientId?: string;
  neonRootDomain?: string;
  slackOAuthClientId?: string;
  slackRootDomain?: string;
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
  isAdminSecretDisabled: boolean;
  consoleSentryDsn?: string; // Corresponds to the HASURA_CONSOLE_SENTRY_DSN environment variable
};

type ProCliEnv = CloudCliEnv;
type ProLiteCliEnv = CloudCliEnv;

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
  projectName?: string;
  cloudRootDomain?: string;
  herokuOAuthClientId?: string;
  luxDataHost?: string;
  schemaRegistryHost: string;
  isAdminSecretSet?: boolean;
  isAdminSecretDisabled?: boolean;
  enableTelemetry?: boolean;
  consoleType?: ConsoleType;
  eeMode?: string | boolean;
  consoleId?: string;
  userRole?: string;
  neonOAuthClientId?: string;
  neonRootDomain?: string;
  slackOAuthClientId?: string;
  slackRootDomain?: string;
  allowedLuxFeatures?: LuxFeature[];
  userId?: string;
  userEmail?: string;
  pricingPlan?: string;
  cdnAssets?: boolean;
  consoleSentryDsn?: string; // Corresponds to the HASURA_CONSOLE_SENTRY_DSN environment variable
  ssoEnabled?: string;
  readonly pro?: boolean;
  readonly hasuraMetricsUrl?: string;
  readonly hasuraOAuthUrl?: string;
  readonly isMetadataAPIEnabled?: boolean;
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
