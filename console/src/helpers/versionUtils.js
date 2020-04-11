import globals from '../Globals';

const semver = require('semver');

export const READ_ONLY_RUN_SQL_QUERIES = 'readOnlyRunSqlQueries';

// list of feature launch versions
const featureLaunchVersions = {
  // feature: 'v1.0.1'
  [READ_ONLY_RUN_SQL_QUERIES]: 'v1.1.0'
};

export const checkValidServerVersion = version => {
  return semver.valid(version) !== null;
};

export const getFeaturesCompatibility = serverVersion => {
  const featuresCompatibility = {};

  const isValidServerVersion = checkValidServerVersion(serverVersion);

  Object.keys(featureLaunchVersions).forEach(feature => {
    featuresCompatibility[feature] = isValidServerVersion
      ? semver.satisfies(serverVersion, '>=' + featureLaunchVersions[feature])
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

export const checkStableVersion = version => {
  try {
    const preReleaseInfo = semver.prerelease(version);

    return preReleaseInfo === null;
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
