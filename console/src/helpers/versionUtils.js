const semver = require('semver');

// list of feature launch versions
const featureLaunchVersions = {
  // feature: 'v1.0.0'
};

export const getFeaturesCompatibility = serverVersion => {
  const featuresCompatibility = {};

  const isPullRequest = serverVersion.startsWith('pull');

  try {
    Object.keys(featureLaunchVersions).forEach(feature => {
      featuresCompatibility[feature] =
        isPullRequest ||
        semver.satisfies(featureLaunchVersions[feature], '>=' + serverVersion);
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
  }
};
