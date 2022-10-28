import { parseConsoleType } from '../envUtils';

describe('parseConsoleType', () => {
  describe('when parseConsoleType is called with env var "oss"', () => {
    it('returns oss', () => {
      expect(parseConsoleType('oss')).toBe('oss');
    });
  });
  describe('when parseConsoleType is called with env var "cloud"', () => {
    it('returns cloud', () => {
      expect(parseConsoleType('cloud')).toBe('cloud');
    });
  });
  describe('when parseConsoleType is called with env var "pro"', () => {
    it('returns pro', () => {
      expect(parseConsoleType('pro')).toBe('pro');
    });
  });
  describe('when parseConsoleType is called with env var "pro-lite"', () => {
    it('returns pro-lite', () => {
      expect(parseConsoleType('pro-lite')).toBe('pro-lite');
    });
  });
  describe('when parseConsoleType is called with env var "invalid"', () => {
    it('returns invalid', () => {
      expect(() => parseConsoleType('invalid')).toThrow(
        'Unmanaged console type "invalid"'
      );
    });
  });
});
