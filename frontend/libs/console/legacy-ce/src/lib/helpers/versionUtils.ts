import globals from '../Globals';

const semver = require('semver');

export const READ_ONLY_RUN_SQL_QUERIES = 'readOnlyRunSqlQueries';
export const INSECURE_TLS_ALLOW_LIST = 'insecureTlsAllowList';

// list of feature launch versions
const featureLaunchVersions = {
  // feature: 'v1.0.1'
  [READ_ONLY_RUN_SQL_QUERIES]: 'v1.1.0',
  [INSECURE_TLS_ALLOW_LIST]: 'v2.0.8',
};

type Feature = keyof typeof featureLaunchVersions;

type FeaturesCompatibility = {
  [key in Feature]?: boolean;
};

export const checkValidServerVersion = (version: string) => {
  return semver.valid(version) !== null;
};

export const getFeaturesCompatibility = (serverVersion: string) => {
  const featuresCompatibility: FeaturesCompatibility = {};

  const isValidServerVersion = checkValidServerVersion(serverVersion);

  Object.keys(featureLaunchVersions).forEach(_feature => {
    const feature = _feature as Feature;
    featuresCompatibility[feature] = isValidServerVersion
      ? semver.satisfies(
          semver.valid(semver.coerce(serverVersion)), // semver.valid(semver.coerce('42.6.7.9.3-alpha')) => '42.6.7'
          `>= ${featureLaunchVersions[feature]}`
        )
      : true;
  });

  return featuresCompatibility;
};

export const versionGT = (version1: string, version2: string) => {
  try {
    return semver.gt(version1, version2);
  } catch (e) {
    console.error(e);
    return false;
  }
};

export const checkStableVersion = (version: string) => {
  try {
    const preReleaseInfo = semver.prerelease(version);

    return preReleaseInfo === null;
  } catch (e) {
    console.error(e);
    return false;
  }
};

export const checkFeatureSupport = (feature: Feature) => {
  return (
    globals.featuresCompatibility && globals.featuresCompatibility[feature]
  );
};
