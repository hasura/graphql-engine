import { hasAdminSecret, parseConsoleType } from './env';

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
        'Unmanaged console type "invalid"',
      );
    });
  });
});

describe('hasAdminSecret', () => {
  it('returns true when the server reports an admin secret', () => {
    expect(hasAdminSecret({ isAdminSecretSet: true })).toBe(true);
  });
  it('returns true when the CLI injects an admin secret', () => {
    expect(hasAdminSecret({ adminSecret: 'cli-secret' })).toBe(true);
  });
  it('returns false when neither is set', () => {
    expect(hasAdminSecret({ isAdminSecretSet: false, adminSecret: '' })).toBe(
      false,
    );
    expect(hasAdminSecret({})).toBe(false);
  });
});
