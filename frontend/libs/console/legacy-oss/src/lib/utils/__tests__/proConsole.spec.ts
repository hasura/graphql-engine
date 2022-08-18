import { isProConsole, ProConsoleEnv } from '../proConsole';

describe('isProConsole', () => {
  describe('when consoleMode is server and consoleType is cloud', () => {
    it('returns true', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'server',
        consoleType: 'cloud',
      };
      expect(isProConsole(env)).toBe(true);
    });
  });

  describe('when consoleMode is server and consoleType is pro', () => {
    it('returns true', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'server',
        consoleType: 'pro',
      };
      expect(isProConsole(env)).toBe(true);
    });
  });

  describe('when consoleMode is cli and pro is true', () => {
    it('returns true', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'cli',
        pro: true,
        consoleType: undefined,
      };
      expect(isProConsole(env)).toBe(true);
    });
  });

  describe('when consoleMode is server and consoleType is pro-lite', () => {
    it('returns true', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'server',
        consoleType: 'pro-lite',
      };
      expect(isProConsole(env)).toBe(true);
    });
  });

  describe('when consoleMode is server and consoleType is oss', () => {
    it('returns false', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'server',
        consoleType: 'oss',
      };
      expect(isProConsole(env)).toBe(false);
    });
  });

  describe('when consoleMode is cli and pro is false', () => {
    it('returns true', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'cli',
        consoleType: undefined,
      };
      expect(isProConsole(env)).toBe(false);
    });
  });
});
