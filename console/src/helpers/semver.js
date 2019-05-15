const semver = require('semver');

const componentsSemver = {
  eventsTab: '1.0.0-alpha16',
  metadataReload: '1.0.0-alpha17',
  eventRedeliver: '1.0.0-alpha17',
  sqlAnalyze: '1.0.0-alpha25',
  aggregationPerm: '1.0.0-alpha26',
  supportColumnChangeTrigger: '1.0.0-alpha26',
  analyzeApiChange: '1.0.0-alpha26',
  insertPrefix: '1.0.0-alpha26',
  insertPermRestrictColumns: '1.0.0-alpha28',
  webhookEnvSupport: '1.0.0-alpha29',
  schemaStitching: '1.0.0-alpha30',
  permHideUpsertSection: '1.0.0-alpha32',
  customFunctionSection: '1.0.0-alpha36',
  triggerRetryTimeout: '1.0.0-alpha38',
  permUpdatePresets: '1.0.0-alpha38',
  tableColumnRename: '1.0.0-alpha39',
  inconsistentState: '1.0.0-alpha43',
  manualTriggers: '1.0.0-alpha46',
};

const getPreRelease = version => {
  const prerelease = semver.prerelease(version);
  if (!prerelease) {
    return '';
  }
  if (prerelease.length === 1) {
    const regex = /(alpha|beta)(\d+)/gm;
    const str = prerelease[0];
    const m = regex.exec(str);
    if (m.length < 3) {
      return '';
    }
    return m.slice(1, 3);
  }
  return prerelease.slice(0, 2);
};

const semverCheck = (component, serverVersion) => {
  if (component in componentsSemver) {
    const componentCoerce = semver.valid(componentsSemver[component]);
    if (componentCoerce == null) {
      return false;
    }

    const serverCoerce = semver.valid(serverVersion);
    if (serverCoerce == null) {
      return true;
    }

    switch (semver.compare(serverCoerce, componentCoerce)) {
      case 0:
        // check for prerelease tags
        const componentPrerelease = getPreRelease(componentsSemver[component]);
        const serverPrerelease = getPreRelease(serverVersion);
        // If both component and server doesn't have a prerelease, return true
        if (componentPrerelease.length === 0 && serverPrerelease.length === 0) {
          return true;
        }
        // If component does't have a prerelease tag and server has a p rerelease
        // tag, return false
        if (componentPrerelease.length === 0 && serverPrerelease.length !== 0) {
          return false;
        }
        // If server does't have a prerelease tag and component has a prerelease
        // tag, return true
        if (componentPrerelease.length !== 0 && serverPrerelease.length === 0) {
          return true;
        }
        if (
          componentPrerelease[0] === 'beta' &&
          serverPrerelease[0] !== 'beta'
        ) {
          return false;
        }
        if (
          parseInt(serverPrerelease[1], 10) >=
          parseInt(componentPrerelease[1], 10)
        ) {
          return true;
        }
        return false;

      case 1:
        return true;
      case -1:
        return false;
      default:
        return false;
    }
  }
  return false;
};

export default semverCheck;
