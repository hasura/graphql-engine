let envObj;

if (process.env.ADMIN_SECRET !== undefined) {
  envObj = `window.__env={
    apiHost: '${process.env.API_HOST}',
    apiPort: '${process.env.API_PORT}',
    dataApiUrl: '${process.env.DATA_API_URL}',
    devDataApiUrl: '${process.env.DEV_DATA_API_URL}',
    adminSecret: '${process.env.ADMIN_SECRET}',
    consoleMode: '${process.env.CONSOLE_MODE}',
    nodeEnv: '${process.env.NODE_ENV}',
    urlPrefix: '${process.env.URL_PREFIX}',
    enableTelemetry: ${process.env.ENABLE_TELEMETRY}
  };`;
} else {
  // ADMIN_SECRET is undefined
  if (process.env.IS_ADMIN_SECRET_SET !== undefined) {
    envObj = `window.__env={
      apiHost: '${process.env.API_HOST}',
      apiPort: '${process.env.API_PORT}',
      dataApiUrl: '${process.env.DATA_API_URL}',
      devDataApiUrl: '${process.env.DEV_DATA_API_URL}',
      isAdminSecretSet: ${process.env.IS_ADMIN_SECRET_SET},
      consoleMode: '${process.env.CONSOLE_MODE}',
      nodeEnv: '${process.env.NODE_ENV}',
      urlPrefix: '${process.env.URL_PREFIX}',
      enableTelemetry: ${process.env.ENABLE_TELEMETRY}
    };`;
  } else {
    // Both ADMIN_SECRET and IS_ADMIN_SECRET_SET is undefined
    if (process.env.ACCESS_KEY !== undefined) {
      envObj = `window.__env={
        apiHost: '${process.env.API_HOST}',
        apiPort: '${process.env.API_PORT}',
        dataApiUrl: '${process.env.DATA_API_URL}',
        devDataApiUrl: '${process.env.DEV_DATA_API_URL}',
        accessKey: ${process.env.ACCESS_KEY},
        consoleMode: '${process.env.CONSOLE_MODE}',
        nodeEnv: '${process.env.NODE_ENV}',
        urlPrefix: '${process.env.URL_PREFIX}',
        enableTelemetry: ${process.env.ENABLE_TELEMETRY}
      };`;
    } else {
      envObj = `window.__env={
        apiHost: '${process.env.API_HOST}',
        apiPort: '${process.env.API_PORT}',
        dataApiUrl: '${process.env.DATA_API_URL}',
        devDataApiUrl: '${process.env.DEV_DATA_API_URL}',
        isAccessKeySet: ${process.env.IS_ACCESS_KEY_SET},
        consoleMode: '${process.env.CONSOLE_MODE}',
        nodeEnv: '${process.env.NODE_ENV}',
        urlPrefix: '${process.env.URL_PREFIX}',
        enableTelemetry: ${process.env.ENABLE_TELEMETRY}
      };`;
    }
  }
}

const env = envObj;

export { env };
