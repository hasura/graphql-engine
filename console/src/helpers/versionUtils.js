const semver = require('semver');

export const FT_JWT_ANALYZER = 'JWTAnalyzer';
export const RELOAD_METADATA_API_CHANGE = 'reloadMetaDataApiChange';
export const REMOTE_SCHEMA_TIMEOUT_CONF_SUPPORT =
  'remoteSchemaTimeoutConfSupport';
export const TABLE_ENUMS_SUPPORT = 'tableEnumsSupport';
export const EXISTS_PERMISSION_SUPPORT = 'existsPermissionSupport';
export const COMPUTED_FIELDS_SUPPORT = 'computedFieldsSupport';

// list of feature launch versions
const featureLaunchVersions = {
  // feature: 'v1.0.0'
  [RELOAD_METADATA_API_CHANGE]: 'v1.0.0-beta.3',
  [FT_JWT_ANALYZER]: 'v1.0.0-beta.3',
  [REMOTE_SCHEMA_TIMEOUT_CONF_SUPPORT]: 'v1.0.0-beta.5',
  [TABLE_ENUMS_SUPPORT]: 'v1.0.0-beta.6',
  [EXISTS_PERMISSION_SUPPORT]: 'v1.0.0-beta.7',
  [COMPUTED_FIELDS_SUPPORT]: 'v1.0.0-beta.8',
};

export const checkValidServerVersion = version => {
  return semver.valid(version) !== null;
};

export const getFeaturesCompatibility = serverVersion => {
  const featuresCompatibility = {};

  const isValidServerVersion = checkValidServerVersion(serverVersion);

  Object.keys(featureLaunchVersions).forEach(feature => {
    featuresCompatibility[feature] = isValidServerVersion
      ? semver.satisfies(featureLaunchVersions[feature], '<=' + serverVersion)
      : true;
  });

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
