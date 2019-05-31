const semver = require('semver');

export const JWT_ANALYZER_VERSION_CHECK = 'JWTAnalyzer';

// list of feature launch versions
const featureLaunchVersions = {
  // feature: 'v1.0.0'
  [JWT_ANALYZER_VERSION_CHECK]: 'v1.0.0-beta.3',
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
