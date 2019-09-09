const semver = require('semver');

export const FT_JWT_ANALYZER = 'JWTAnalyzer';
export const RELOAD_METADATA_API_CHANGE = 'reloadMetaDataApiChange';
export const REMOTE_SCHEMA_TIMEOUT_CONF_SUPPORT =
  'remoteSchemaTimeoutConfSupport';
export const TABLE_ENUMS_SUPPORT = 'tableEnumsSupport';

// list of feature launch versions
const featureLaunchVersions = {
  // feature: 'v1.0.0'
  [RELOAD_METADATA_API_CHANGE]: 'v1.0.0-beta.3',
  [FT_JWT_ANALYZER]: 'v1.0.0-beta.3',
  [REMOTE_SCHEMA_TIMEOUT_CONF_SUPPORT]: 'v1.0.0-beta.5',
  [TABLE_ENUMS_SUPPORT]: 'v1.0.0-beta.6'
};

export const getFeaturesCompatibility = serverVersion => {
  const featuresCompatibility = {};

  const isPullRequest = serverVersion.startsWith('pull');

  try {
    Object.keys(featureLaunchVersions).forEach(feature => {
      featuresCompatibility[feature] =
        isPullRequest ||
        semver.satisfies(featureLaunchVersions[feature], '<=' + serverVersion);
    });
  } catch (e) {
    console.error(e);
  }

  return featuresCompatibility;
};

export const versionGT = (version1, version2) => {
  try {
    return semver.gt(version1, version2);
  } catch (e) {
    console.error(e);
    return false;
  }
};
