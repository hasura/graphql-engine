import type { EnvVars } from '../../../../Globals';

/**
 * Return the tags to be used in Sentry.
 *
 * ATTENTION: To avoid leaking sensitive data, it's better to whitelist vars instead of
 * blacklisting them. It would be easier to filter out the adminSecret and tracking whatever
 * else, but what happens if in the future some more secret-like vars will be added? They would
 * accidentally be sent to Sentry, something that we should avoid.
 */
export function getSentryTags(envVars: EnvVars) {
  if (envVars.consoleMode === 'cli') {
    if ('pro' in envVars) {
      return {
        pro: envVars.pro,
        apiHost: envVars.apiHost,
        apiPort: envVars.apiPort,
        cliUUID: envVars.cliUUID,
        urlPrefix: envVars.urlPrefix,
        projectId: envVars.projectId,
        assetsPath: envVars.assetsPath,
        dataApiUrl: envVars.dataApiUrl,
        consoleMode: envVars.consoleMode,
        adminSecret: envVars.adminSecret,
        consolePath: envVars.consolePath,
        serverVersion: envVars.serverVersion,
        enableTelemetry: envVars.enableTelemetry,
        isAdminSecretSet: envVars.isAdminSecretSet,
      };
    }

    return {
      apiHost: envVars.apiHost,
      apiPort: envVars.apiPort,
      cliUUID: envVars.cliUUID,
      urlPrefix: envVars.urlPrefix,
      assetsPath: envVars.assetsPath,
      dataApiUrl: envVars.dataApiUrl,
      consoleMode: envVars.consoleMode,
      consolePath: envVars.consolePath,
      serverVersion: envVars.serverVersion,
      enableTelemetry: envVars.enableTelemetry,
    };
  }

  switch (envVars.consoleType) {
    case 'oss':
      return {
        urlPrefix: envVars.urlPrefix,
        cdnAssets: envVars.cdnAssets,
        assetsPath: envVars.assetsPath,
        consoleMode: envVars.consoleMode,
        consoleType: envVars.consoleType,
        consolePath: envVars.consolePath,
        serverVersion: envVars.serverVersion,
        enableTelemetry: envVars.enableTelemetry,
        isAdminSecretSet: envVars.isAdminSecretSet,
      };

    case 'pro':
      return {
        consoleId: envVars.consoleId,
        urlPrefix: envVars.urlPrefix,
        assetsPath: envVars.assetsPath,
        consoleType: envVars.consoleType,
        consoleMode: envVars.consoleMode,
        consolePath: envVars.consolePath,
        serverVersion: envVars.serverVersion,
        enableTelemetry: envVars.enableTelemetry,
        isAdminSecretSet: envVars.isAdminSecretSet,
      };

    case 'cloud':
      return {
        eeMode: envVars.eeMode,
        tenantID: envVars.tenantID,
        userRole: envVars.userRole,
        consoleId: envVars.consoleId,
        projectID: envVars.projectID,
        urlPrefix: envVars.urlPrefix,
        assetsPath: envVars.assetsPath,
        dataApiUrl: envVars.dataApiUrl,
        consoleMode: envVars.consoleMode,
        consoleType: envVars.consoleType,
        consolePath: envVars.consolePath,
        luxDataHost: envVars.luxDataHost,
        serverVersion: envVars.serverVersion,
        cloudRootDomain: envVars.cloudRootDomain,
        isAdminSecretSet: envVars.isAdminSecretSet,
        herokuOAuthClientId: envVars.herokuOAuthClientId,
      };

    default:
      console.warn('Unknown Console version');

      return {
        // This is a fallback it should never happen. If it happens, the above cases should be extended.
        unknownConsole: true,

        consoleMode: envVars.consoleMode,
        consoleType: envVars.consoleType,
      };
  }
}
