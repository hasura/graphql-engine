import { getPathRoot } from '../urlUtils';

describe('getPathRoot', () => {
  it.each`
    path
    ${undefined}
    ${null}
    ${''}
  `('Given a falsy value ($path), then returns a empty string', ({ path }) => {
    expect(getPathRoot(path)).toBe('');
  });

  it.each`
    path              | expected
    ${'/foo/bar/doe'} | ${'foo'}
    ${'/foo/bar'}     | ${'foo'}
    ${'/foo'}         | ${'foo'}
    ${'/'}            | ${''}
  `('Given a $path path, then returns $expected', ({ path, expected }) => {
    expect(getPathRoot(path)).toBe(expected);
  });
});
