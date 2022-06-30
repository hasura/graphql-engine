import { getPathRoot } from '../urlUtils';

describe('getPathRoot', () => {
  describe('returns empty string with falsy paths', () => {
    it.each([
      {
        path: undefined,
        expected: '',
      },
      {
        path: null,
        expected: '',
      },
      {
        path: '',
        expected: '',
      },
    ])('for %o', ({ path, expected }) => {
      expect(getPathRoot(path)).toBe(expected);
    });
  });

  describe('returns the first part of the path', () => {
    it.each([
      {
        path: '/foo/bar/doe',
        expected: 'foo',
      },
      {
        path: '/foo/bar',
        expected: 'foo',
      },
      {
        path: '/foo',
        expected: 'foo',
      },
      {
        path: '/',
        expected: '',
      },
    ])('for %o', ({ path, expected }) => {
      expect(getPathRoot(path)).toBe(expected);
    });
  });
});
