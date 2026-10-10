import type { CloudCliEnv } from '@hasura/shared/types';
import { getSentryTags } from './getSentryTags';

// Regression for the pro/cloud CLI branch of getSentryTags: the function
// whitelists the vars forwarded to Sentry, but that branch used to include the
// raw `adminSecret`. The secret must never reach Sentry; only the boolean
// `isAdminSecretSet` flag may be forwarded. These tests cover ONLY that branch
// (consoleMode: 'cli' + a `projectId`, i.e. CloudCliEnv / ProCliEnv).
describe('getSentryTags — pro/cloud CLI branch', () => {
  // A CloudCliEnv carrying a real admin secret value.
  const cloudCliEnv: CloudCliEnv = {
    consoleMode: 'cli',
    adminSecret: 'super-secret-admin-secret',
    apiHost: 'http://localhost',
    apiPort: '9693',
    assetsPath: '/assets',
    cliUUID: 'cli-uuid',
    consolePath: '/console',
    dataApiUrl: 'http://localhost:8080',
    enableTelemetry: true,
    serverVersion: 'v2.0.0',
    urlPrefix: '/',
    pro: true,
    projectId: '00000000-0000-0000-0000-000000000000',
    isAdminSecretSet: true,
    isAdminSecretDisabled: false,
  };

  it('does NOT forward the admin secret to Sentry', () => {
    const tags = getSentryTags(cloudCliEnv);

    expect(tags).not.toHaveProperty('adminSecret');
    expect(Object.values(tags)).not.toContain(cloudCliEnv.adminSecret);
  });

  it('preserves the existing safe output contract (projectId + isAdminSecretSet)', () => {
    const tags = getSentryTags(cloudCliEnv);

    // The output `projectId` key is a runtime contract and must be kept.
    expect(tags).toHaveProperty('projectId', cloudCliEnv.projectId);
    // The boolean flag conveys the admin-secret state without leaking it.
    expect(tags).toHaveProperty('isAdminSecretSet', true);
  });
});
