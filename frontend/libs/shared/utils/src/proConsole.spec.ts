import { EnvVars } from '@hasura/shared/types';
import {
  getProjectId,
  isMonitoringTabSupportedEnvironment,
  isProConsole,
} from './proConsole';

describe('isProConsole', () => {
  describe('when consoleMode is server and consoleType is cloud', () => {
    it('returns true', () => {
      const env = {
        consoleMode: 'server',
        consoleType: 'cloud',
      } as EnvVars;
      expect(isProConsole(env)).toBe(true);
    });
  });

  describe('when consoleMode is server and consoleType is pro', () => {
    it('returns true', () => {
      const env = {
        consoleMode: 'server',
        consoleType: 'pro',
      } as EnvVars;
      expect(isProConsole(env)).toBe(true);
    });
  });

  describe('when consoleMode is cli and pro is true', () => {
    it('returns true', () => {
      const env = {
        consoleMode: 'cli',
        pro: true,
        consoleType: undefined,
      } as EnvVars;
      expect(isProConsole(env)).toBe(true);
    });
  });

  describe('when consoleMode is server and consoleType is pro-lite', () => {
    it('returns true', () => {
      const env = {
        consoleMode: 'server',
        consoleType: 'pro-lite',
      } as EnvVars;
      expect(isProConsole(env)).toBe(false);
    });
  });

  describe('when consoleMode is server and consoleType is oss', () => {
    it('returns false', () => {
      const env = {
        consoleMode: 'server',
        consoleType: 'oss',
      } as EnvVars;
      expect(isProConsole(env)).toBe(false);
    });
  });

  describe('when consoleMode is cli and pro is false', () => {
    it('returns true', () => {
      const env = {
        consoleMode: 'cli',
        consoleType: undefined,
      } as EnvVars;
      expect(isProConsole(env)).toBe(false);
    });
  });

  describe('when consoleMode is cli and consoleType is cloud', () => {
    it('returns true', () => {
      const env = {
        consoleMode: 'cli',
        consoleType: 'cloud',
      } as EnvVars;
      expect(isProConsole(env)).toBe(true);
    });
  });

  describe('when consoleMode is cli and consoleType is pro', () => {
    it('returns true', () => {
      const env = {
        consoleMode: 'cli',
        consoleType: 'pro',
      } as EnvVars;
      expect(isProConsole(env)).toBe(true);
    });
  });

  describe('when consoleMode is cli and consoleType is pro-lite', () => {
    it('returns true', () => {
      const env = {
        consoleMode: 'cli',
        consoleType: 'pro-lite',
      } as EnvVars;
      expect(isProConsole(env)).toBe(false);
    });
  });

  describe('when consoleMode is cli and consoleType is oss', () => {
    it('returns false', () => {
      const env = {
        consoleMode: 'cli',
        consoleType: 'oss',
      } as EnvVars;
      expect(isProConsole(env)).toBe(false);
    });
  });
});

describe('isMonitoringTabSupportedEnvironment', () => {
  // Server Runtimes
  describe('when consoleMode is server and consoleType is cloud (ie. Production cloud runtime)', () => {
    it('returns true', () => {
      const env = {
        consoleMode: 'server',
        consoleType: 'cloud',
      } as EnvVars;
      expect(isMonitoringTabSupportedEnvironment(env)).toBe(true);
    });
  });

  describe('when consoleMode is server and consoleType is pro (ie. Self hosted runtime)', () => {
    it('returns true', () => {
      const env = {
        consoleMode: 'server',
        consoleType: 'pro',
      } as EnvVars;
      expect(isMonitoringTabSupportedEnvironment(env)).toBe(true);
    });
  });
  describe('when consoleMode is server and consoleType is pro-lite', () => {
    it('returns false', () => {
      const env = {
        consoleMode: 'server',
        consoleType: 'pro-lite',
      } as EnvVars;
      expect(isMonitoringTabSupportedEnvironment(env)).toBe(false);
    });
  });

  describe('when consoleMode is server and consoleType is oss', () => {
    it('returns false', () => {
      const env = {
        consoleMode: 'server',
        consoleType: 'oss',
      } as EnvVars;
      expect(isMonitoringTabSupportedEnvironment(env)).toBe(false);
    });
  });

  // CLI runtimes
  // Cloud and Self hosted EE (with LUX)
  describe('when consoleMode is cli and pro is true', () => {
    it('returns true', () => {
      const env = {
        consoleMode: 'cli',
        pro: true,
        consoleType: undefined,
      } as EnvVars;
      expect(isMonitoringTabSupportedEnvironment(env)).toBe(true);
    });
  });

  // OSS console CLI
  describe('when consoleMode is cli and consoleType is oss', () => {
    it('returns false', () => {
      const env = {
        consoleMode: 'cli',
        consoleType: 'oss',
      } as EnvVars;
      expect(isMonitoringTabSupportedEnvironment(env)).toBe(false);
    });
  });

  // EE lite CLI mode
  describe('when consoleMode is cli and pro is undefined', () => {
    it('returns false', () => {
      const env = {
        consoleMode: 'cli',
      } as EnvVars;
      expect(isMonitoringTabSupportedEnvironment(env)).toBe(false);
    });
  });
});

describe('getProjectId', () => {
  // Hasura Cloud (server runtime): the project id lives on the canonical
  // `projectID` (capital ID) field and is only returned for a real Cloud
  // console (tenantID present + consoleType 'cloud').
  it('returns the projectID for a Hasura Cloud server console', () => {
    const env = {
      consoleMode: 'server',
      consoleType: 'cloud',
      tenantID: 'tenant-1',
      projectID: 'project-1',
    } as EnvVars;
    expect(getProjectId(env)).toBe('project-1');
  });

  it('returns undefined for a Cloud console whose projectID is absent', () => {
    const env = {
      consoleMode: 'server',
      consoleType: 'cloud',
      tenantID: 'tenant-1',
      // projectID intentionally absent
    } as EnvVars;
    expect(getProjectId(env)).toBeUndefined();
  });

  it('returns undefined for Hasura EE cloud (consoleType cloud but no tenantID)', () => {
    const env = {
      consoleMode: 'server',
      consoleType: 'cloud',
      projectID: 'project-1',
      // tenantID intentionally absent -> not a Cloud console
    } as EnvVars;
    expect(getProjectId(env)).toBeUndefined();
  });

  it('returns undefined for an OSS server console', () => {
    const env = {
      consoleMode: 'server',
      consoleType: 'oss',
    } as EnvVars;
    expect(getProjectId(env)).toBeUndefined();
  });

  it('returns undefined for a pro (self-hosted) server console', () => {
    const env = {
      consoleMode: 'server',
      consoleType: 'pro',
      projectID: 'project-1',
    } as EnvVars;
    expect(getProjectId(env)).toBeUndefined();
  });

  // CLI runtimes carry the project id on the lowercase `projectId` field and do
  // not report consoleType 'cloud', so they are not treated as a Cloud console
  // by getProjectId (which gates on isCloudConsole).
  it('returns undefined for a pro CLI console (projectId, no cloud consoleType)', () => {
    const env = {
      consoleMode: 'cli',
      pro: true,
      projectId: 'project-cli',
    } as EnvVars;
    expect(getProjectId(env)).toBeUndefined();
  });

  // Canonical casing guard: getProjectId reads `projectID` (capital). An env
  // that only exposes the lowercase `projectId` must NOT be mistaken for the
  // Cloud project id.
  it('does not read the lowercase projectId field', () => {
    const env = {
      consoleMode: 'server',
      consoleType: 'cloud',
      tenantID: 'tenant-1',
      projectId: 'wrong-casing',
    } as unknown as EnvVars;
    expect(getProjectId(env)).toBeUndefined();
  });
});
