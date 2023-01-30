import { parseSentryDsn } from './parseSentryDsn';

describe('parseSentryDsn', () => {
  it.each`
    value                                 | expected
    ${''}                                 | ${{ status: 'missing' }}
    ${null}                               | ${{ status: 'missing' }}
    ${undefined}                          | ${{ status: 'missing' }}
    ${0}                                  | ${{ status: 'invalid', value: 0 }}
    ${1}                                  | ${{ status: 'invalid', value: 1 }}
    ${{}}                                 | ${{ status: 'invalid', value: {} }}
    ${[]}                                 | ${{ status: 'invalid', value: [] }}
    ${true}                               | ${{ status: 'invalid', value: true }}
    ${false}                              | ${{ status: 'invalid', value: false }}
    ${'https://sentry.io'}                | ${{ status: 'invalid', value: 'https://sentry.io' }}
    ${'https://ingest.sentry.io'}         | ${{ status: 'invalid', value: 'https://ingest.sentry.io' }}
    ${'https://ingest.sentry.io'}         | ${{ status: 'invalid', value: 'https://ingest.sentry.io' }}
    ${'https://foo.ingest.sentry.io'}     | ${{ status: 'invalid', value: 'https://foo.ingest.sentry.io' }}
    ${'https://foo.ingest.sentry.io/'}    | ${{ status: 'invalid', value: 'https://foo.ingest.sentry.io/' }}
    ${'https://.ingest.sentry.io/bar'}    | ${{ status: 'invalid', value: 'https://.ingest.sentry.io/bar' }}
    ${'http://foo.ingest.sentry.io/bar'}  | ${{ status: 'invalid', value: 'http://foo.ingest.sentry.io/bar' }}
    ${'https://foo.ingest.sentry.io/bar'} | ${{ status: 'valid', value: 'https://foo.ingest.sentry.io/bar' }}
  `(
    `When invoked with '$value', then should return '$expected'`,
    ({ value, expected }) => {
      expect(parseSentryDsn(value)).toEqual(expected);
    }
  );
});
