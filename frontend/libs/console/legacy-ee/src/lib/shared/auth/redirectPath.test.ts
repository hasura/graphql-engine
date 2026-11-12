import { getLoginRedirectPath } from './redirectPath';

const prefix = '/project/1685116d-7f77-4d37-8efe-6cd28de0b010/console';

describe('getLoginRedirectPath', () => {
  it.each([undefined, null, '', 'undefined', 'null'])(
    'defaults %s to the console root',
    (path) => expect(getLoginRedirectPath(path, prefix)).toBe('/'),
  );

  it.each(['', '/', '/console', prefix, `${prefix}/`])(
    'does not add the BrowserRouter basename (%s)',
    (basename) => {
      expect(getLoginRedirectPath('/data?source=default#table', basename)).toBe(
        '/data?source=default#table',
      );
      expect(getLoginRedirectPath('/', basename)).toBe('/');
    },
  );

  it('strips an existing basename on a segment boundary', () => {
    expect(getLoginRedirectPath(prefix, prefix)).toBe('/');
    expect(getLoginRedirectPath(`${prefix}/data?x=1#table`, prefix)).toBe(
      '/data?x=1#table',
    );
    expect(getLoginRedirectPath(`${prefix}?x=1`, prefix)).toBe('/?x=1');
    expect(getLoginRedirectPath(`${prefix}#table`, prefix)).toBe('/#table');
    expect(getLoginRedirectPath(`${prefix}-other`, prefix)).toBe(
      `${prefix}-other`,
    );
  });

  it.each([
    'https://example.com',
    '//example.com',
    '/\\\\example.com',
    '/data\n',
    '/login?auto_login=true',
    '/oauth2/callback?code=old',
  ])('rejects unsafe or recursive targets: %s', (path) => {
    expect(getLoginRedirectPath(path, prefix)).toBe('/');
  });
});
