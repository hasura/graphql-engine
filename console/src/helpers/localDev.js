let envObj = `
      apiHost: '${process.env.API_HOST}',
      apiPort: '${process.env.API_PORT}',
      dataApiUrl: '${process.env.DATA_API_URL}',
      consoleMode: '${process.env.CONSOLE_MODE}',
      nodeEnv: '${process.env.NODE_ENV}',
      urlPrefix: '${process.env.URL_PREFIX}',
      enableTelemetry: ${process.env.ENABLE_TELEMETRY},
      assetsPath: '${process.env.ASSETS_PATH}',
      assetsVersion: '${process.env.ASSETS_VERSION}',
      serverVersion: '${process.env.SERVER_VERSION}',
      cdnAssets: ${process.env.CDN_ASSETS},`;

if (process.env.ADMIN_SECRET !== undefined) {
  envObj += `
      adminSecret: '${process.env.ADMIN_SECRET}',`;
} else {
  // ADMIN_SECRET is undefined
  if (process.env.IS_ADMIN_SECRET_SET !== undefined) {
    envObj += `
      isAdminSecretSet: ${process.env.IS_ADMIN_SECRET_SET},`;
  }
}

const env = `
    window.__env={
      ${envObj}
    };
`;

export { env };
