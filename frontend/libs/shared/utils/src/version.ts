import semver from 'semver';

// list of feature launch versions
const featureLaunchVersions = {
  // feature: 'v1.0.1'
  readOnlyRunSqlQueries: 'v1.1.0',
};

export type Feature = keyof typeof featureLaunchVersions;

export type FeaturesCompatibility = {
  [key in Feature]?: boolean;
};

export const checkValidServerVersion = (version: string) => {
  return semver.valid(version) !== null;
};

export const getFeaturesCompatibility = (serverVersion: string) => {
  const featuresCompatibility: FeaturesCompatibility = {};

  const isValidServerVersion = checkValidServerVersion(serverVersion);

  Object.keys(featureLaunchVersions).forEach((_feature) => {
    const feature = _feature as Feature;
    const coerceSemver = semver.valid(semver.coerce(serverVersion));
    featuresCompatibility[feature] =
      coerceSemver && isValidServerVersion
        ? semver.satisfies(
            coerceSemver, // semver.valid(semver.coerce('42.6.7.9.3-alpha')) => '42.6.7'
            `>= ${featureLaunchVersions[feature]}`,
          )
        : true;
  });

  return featuresCompatibility;
};

export const versionGT = (version1: string, version2: string) => {
  if (!version2) {
    return true;
  }

  if (!version1) {
    return false;
  }

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
