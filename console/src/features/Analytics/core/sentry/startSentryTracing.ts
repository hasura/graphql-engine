import type { EnvVars } from '@/Globals';

import * as Sentry from '@sentry/react';
import { BrowserTracing } from '@sentry/tracing';

import globals from '@/Globals';
import { getSentryTags } from './getSentryTags';
import { getSentryEnvironment } from './getSentryEnvironment';
import { isSentryAlreadyStarted } from './isSentryAlreadyStarted';
import { logSentryEnabled, logSentryDisabled } from './logSentryInfo';

type Globals = typeof globals;

/**
 * Start Sentry idempotently.
 *
 * Please note that Sentry automatically tracks also the React errors, there is no need to manually track them
 * from the various React error boundaries.
 *
 * ATTENTION: This function expects the  `window.__envVars` because I think
 * using the server-driven vars instead of the client-parsed ones (since they could
 * differ in some details) as tags would be better.
 */
export function startSentryTracing(globalVars: Globals, envVars: EnvVars) {
  if (isSentryAlreadyStarted()) return 'enabled';

  const consoleSentryDsn = globalVars.consoleSentryDsn;
  if (
    consoleSentryDsn.status === 'missing' ||
    consoleSentryDsn.status === 'invalid'
  ) {
    logSentryDisabled();
    return 'disabled';
  }

  const tags = getSentryTags(envVars);
  const environment = getSentryEnvironment(window.location.hostname);

  logSentryEnabled(environment);

  Sentry.init({
    dsn: consoleSentryDsn.value,
    tracesSampleRate: 1.0,
    integrations: [
      new BrowserTracing(),

      new Sentry.Integrations.Breadcrumbs({
        // Disable tracking console.logs
        console: false,

        // Disable tracking clicks, important to avoid leaking sensitive data.
        // TODO:
        // 1. Check what kind selectors Sentry generates, maybe we could enable it right now...
        // 2. If not... we could reenable it once we are sure no HTML attributes contain
        // sensitive data
        dom: false,
      }),
    ],

    // Allow grouping logs by environment
    environment,
    release: tags.serverVersion,
    initialScope: {
      tags,
    },
  });

  Sentry.setUser({
    id: globalVars.userId,
    ip_address: '{{auto}}',
  });

  return 'enabled';
}
