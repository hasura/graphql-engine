import { getSentryEnvironment } from './getSentryEnvironment';

describe('getSentryEnvironment', () => {
  it.each`
    hostname                            | expectedEnvironment
    ${'localhost'}                      | ${'local'}
    ${'stagingHostname'}                | ${'stagingHostname'}
    ${'unmanagedHostname'}              | ${'unmanagedHostname'}
    ${'hge-mono-pr-3792.herokuapp.com'} | ${'hge-mono-pr.herokuapp.com'}
  `(
    `When invoked with '$hostname', then should return '$expectedEnvironment'`,
    ({ hostname, expectedEnvironment }) => {
      expect(getSentryEnvironment(hostname)).toEqual(expectedEnvironment);
    }
  );
});
