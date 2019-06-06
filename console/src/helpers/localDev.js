const envObj = `apiHost: '${process.env.API_HOST}',
        apiPort: '${process.env.API_PORT}',
        dataApiUrl: '${process.env.DATA_API_URL}',
        adminSecret: '${process.env.ADMIN_SECRET}',
        consoleMode: '${process.env.CONSOLE_MODE}',
        nodeEnv: '${process.env.NODE_ENV}',
        urlPrefix: '${process.env.URL_PREFIX}',
        enableTelemetry: ${process.env.ENABLE_TELEMETRY},
        assetsPath: '${process.env.ASSETS_PATH}',
        assetsVersion: '${process.env.ASSETS_VERSION}',
        cdnAssets: ${process.env.CDN_ASSETS},
`;

let appendObj;

if (process.env.ADMIN_SECRET !== undefined) {
  appendObj = `
  adminSecret: '${process.env.ADMIN_SECRET}`;
} else {
  // ADMIN_SECRET is undefined
  if (process.env.IS_ADMIN_SECRET_SET !== undefined) {
    appendObj = `isAdminSecretSet: ${process.env.IS_ADMIN_SECRET_SET}`;
  } else {
    // Both ADMIN_SECRET and IS_ADMIN_SECRET_SET is undefined
    if (process.env.ACCESS_KEY !== undefined) {
      appendObj = `accessKey: ${process.env.ACCESS_KEY}`;
    } else {
      appendObj = `isAccessKeySet: ${process.env.IS_ACCESS_KEY_SET}`;
    }
  }
}

const env = `
    window.__env={\n\t\t${envObj}\t\t${appendObj}
    };
`;

export { env };
