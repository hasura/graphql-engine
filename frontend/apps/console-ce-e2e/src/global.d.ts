interface Env {
  adminSecret: string;
  apiHost: string;
  apiPort: string;
  assetsPath: string;
  assetsVersion: string;
  assetVersion: string;
  cdnAssets: boolean;
  consoleAssetVersion: string;
  consoleMode: string;
  consolePath: string;
  dataApiUrl: string;
  enableTelemetry: boolean;
  featuresCompatibility: string;
  isAdminSecretSet: boolean;
  isproduction: boolean;
  nodeEnv: string;
  serverVersion: string;
  telemetryTopic: string;
  urlPrefix: string;
}

interface Window {
  __env: Env;
}
