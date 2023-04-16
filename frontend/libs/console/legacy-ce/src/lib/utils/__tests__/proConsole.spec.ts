import {
  isEnvironmentSupportMultiTenantConnectionPooling,
  isMonitoringTabSupportedEnvironment,
  isProConsole,
  ProConsoleEnv,
} from '../proConsole';

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
      expect(isProConsole(env)).toBe(false);
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

  describe('when consoleMode is cli and consoleType is cloud', () => {
    it('returns true', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'cli',
        consoleType: 'cloud',
      };
      expect(isProConsole(env)).toBe(true);
    });
  });

  describe('when consoleMode is cli and consoleType is pro', () => {
    it('returns true', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'cli',
        consoleType: 'pro',
      };
      expect(isProConsole(env)).toBe(true);
    });
  });

  describe('when consoleMode is cli and consoleType is pro-lite', () => {
    it('returns true', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'cli',
        consoleType: 'pro-lite',
      };
      expect(isProConsole(env)).toBe(false);
    });
  });

  describe('when consoleMode is cli and consoleType is oss', () => {
    it('returns false', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'cli',
        consoleType: 'oss',
      };
      expect(isProConsole(env)).toBe(false);
    });
  });
});

describe('isMonitoringTabSupportedEnvironment', () => {
  // Server Runtimes
  describe('when consoleMode is server and consoleType is cloud (ie. Production cloud runtime)', () => {
    it('returns true', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'server',
        consoleType: 'cloud',
      };
      expect(isMonitoringTabSupportedEnvironment(env)).toBe(true);
    });
  });

  describe('when consoleMode is server and consoleType is pro (ie. Self hosted runtime)', () => {
    it('returns true', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'server',
        consoleType: 'pro',
      };
      expect(isMonitoringTabSupportedEnvironment(env)).toBe(true);
    });
  });
  describe('when consoleMode is server and consoleType is pro-lite', () => {
    it('returns false', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'server',
        consoleType: 'pro-lite',
      };
      expect(isMonitoringTabSupportedEnvironment(env)).toBe(false);
    });
  });

  describe('when consoleMode is server and consoleType is oss', () => {
    it('returns false', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'server',
        consoleType: 'oss',
      };
      expect(isMonitoringTabSupportedEnvironment(env)).toBe(false);
    });
  });

  // CLI runtimes
  // Cloud and Self hosted EE (with LUX)
  describe('when consoleMode is cli and pro is true', () => {
    it('returns true', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'cli',
        pro: true,
        consoleType: undefined,
      };
      expect(isMonitoringTabSupportedEnvironment(env)).toBe(true);
    });
  });

  // OSS console CLI
  describe('when consoleMode is cli and consoleType is oss', () => {
    it('returns false', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'cli',
        consoleType: 'oss',
      };
      expect(isMonitoringTabSupportedEnvironment(env)).toBe(false);
    });
  });

  // EE lite CLI mode
  describe('when consoleMode is cli and pro is undefined', () => {
    it('returns false', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'cli',
      };
      expect(isMonitoringTabSupportedEnvironment(env)).toBe(false);
    });
  });
});

describe('isEnvironmentSupportMultiTenantConnectionPooling', () => {
  // Server Runtimes
  describe('when consoleMode is server and consoleType is cloud (ie. Production cloud runtime)', () => {
    it('returns true', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'server',
        consoleType: 'cloud',
      };
      expect(isEnvironmentSupportMultiTenantConnectionPooling(env)).toBe(true);
    });
  });

  describe('when consoleMode is server and consoleType is pro (ie. Self hosted runtime)', () => {
    it('returns true', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'server',
        consoleType: 'pro',
      };
      expect(isEnvironmentSupportMultiTenantConnectionPooling(env)).toBe(false);
    });
  });
  describe('when consoleMode is server and consoleType is pro-lite', () => {
    it('returns false', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'server',
        consoleType: 'pro-lite',
      };
      expect(isEnvironmentSupportMultiTenantConnectionPooling(env)).toBe(false);
    });
  });

  describe('when consoleMode is server and consoleType is oss', () => {
    it('returns false', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'server',
        consoleType: 'oss',
      };
      expect(isEnvironmentSupportMultiTenantConnectionPooling(env)).toBe(false);
    });
  });

  // CLI runtimes
  // Cloud and Self hosted EE (with LUX)
  describe('when consoleMode is cli and pro is true', () => {
    it('returns true', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'cli',
        pro: true,
        consoleType: undefined,
      };
      expect(isEnvironmentSupportMultiTenantConnectionPooling(env)).toBe(true);
    });
  });

  // OSS console CLI
  describe('when consoleMode is cli and consoleType is oss', () => {
    it('returns false', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'cli',
        consoleType: 'oss',
      };
      expect(isEnvironmentSupportMultiTenantConnectionPooling(env)).toBe(false);
    });
  });

  // EE lite CLI mode
  describe('when consoleMode is cli and pro is undefined', () => {
    it('returns false', () => {
      const env: ProConsoleEnv = {
        consoleMode: 'cli',
      };
      expect(isEnvironmentSupportMultiTenantConnectionPooling(env)).toBe(false);
    });
  });
});
