export function getSentryEnvironment(windowLocationHostname: string) {
  if (windowLocationHostname === 'localhost') {
    return 'local';
  }

  if (windowLocationHostname.startsWith('hge-mono-pr')) {
    // Allow grouping all the PRs under the same environment in Sentry
    return 'hge-mono-pr.herokuapp.com';
  }

  /*
  Please note that returning the hostname allows
  1. Not to expose the staging URL in the OSS repo
  2. Easily detect unmanaged hosts in Sentry
  */
  return windowLocationHostname;
}
