const semver = require('semver');

// list of feature launch versions
const featureVersions = {
  // feature: 'v1.0.0'
};

export const getFeaturesSupport = serverVersion => {
  const featuresSupport = {};

  Object.keys(featureVersions).forEach(feature => {
    try {
      featuresSupport[feature] = semver.satisfies(
        featureVersions[feature],
        '>=' + serverVersion
      );
    } catch (e) {
      console.log(e);
    }
  });

  return featuresSupport;
};

export const versionGT = (version1, version2) => {
  try {
    return semver.gt(version1, version2);
  } catch (e) {
    console.log(e);
  }
};
