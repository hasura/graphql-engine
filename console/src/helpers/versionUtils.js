import globals from '../Globals';

const semver = require('semver');

export const FT_JWT_ANALYZER = 'JWTAnalyzer';
export const RELOAD_METADATA_API_CHANGE = 'reloadMetaDataApiChange';
export const REMOTE_SCHEMA_TIMEOUT_CONF_SUPPORT =
  'remoteSchemaTimeoutConfSupport';
export const TABLE_ENUMS_SUPPORT = 'tableEnumsSupport';
export const EXISTS_PERMISSION_SUPPORT = 'existsPermissionSupport';
export const CUSTOM_GRAPHQL_FIELDS_SUPPORT = 'customGraphQLFieldsSupport';
export const COMPUTED_FIELDS_SUPPORT = 'computedFieldsSupport';
export const IMPROVED_EVENT_FETCH_QUERY = 'improvedEventFetchQuery';

// list of feature launch versions
const featureLaunchVersions = {
  // feature: 'v1.0.0'
  [RELOAD_METADATA_API_CHANGE]: 'v1.0.0-beta.3',
  [FT_JWT_ANALYZER]: 'v1.0.0-beta.3',
  [REMOTE_SCHEMA_TIMEOUT_CONF_SUPPORT]: 'v1.0.0-beta.5',
  [TABLE_ENUMS_SUPPORT]: 'v1.0.0-beta.6',
  [EXISTS_PERMISSION_SUPPORT]: 'v1.0.0-beta.7',
  [CUSTOM_GRAPHQL_FIELDS_SUPPORT]: 'v1.0.0-beta.8',
  [COMPUTED_FIELDS_SUPPORT]: 'v1.0.0-beta.8',
  [IMPROVED_EVENT_FETCH_QUERY]: 'v1.0.0-beta.10',
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

export const checkFeatureSupport = feature => {
  return (
    globals.featuresCompatibility && globals.featuresCompatibility[feature]
  );
};
