import type { EnvVars } from '@hasura/shared/types';
import * as Sentry from '@sentry/react';
import { getSentryTags } from './getSentryTags';
import { errorMustBeBlocked } from './errorMustBeBlocked';
import { getSentryEnvironment } from './getSentryEnvironment';
import { isSentryAlreadyStarted } from './isSentryAlreadyStarted';
import { logSentryEnabled, logSentryDisabled } from './logSentryInfo';
import { parseSentryDsn } from './parseSentryDsn';

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
export function startSentryTracing(envVars: EnvVars) {
  if (isSentryAlreadyStarted()) return 'enabled';

  const consoleSentryDsn = parseSentryDsn(envVars.consoleSentryDsn);
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
    integrations: (defaultIntegrations) => [
      // Disable tracking console.logs. Console breadcrumbs come from their own
      // default integration since Sentry v11.
      ...defaultIntegrations.filter(
        (integration) => integration.name !== 'Console',
      ),

      Sentry.browserTracingIntegration(),

      Sentry.breadcrumbsIntegration({
        // Disable tracking clicks, important to avoid leaking sensitive data.
        // TODO:
        // 1. Check what kind selectors Sentry generates, maybe we could enable it right now...
        // 2. If not... we could reenable it once we are sure no HTML attributes contain
        // sensitive data
        dom: false,
      }),

      // ATTENTION: functions like programmaticallyTraceError could internally log errors to the
      // browser's console, causing an infinite loop!
      // Sentry.captureConsoleIntegration({
      //   levels: ['error'],
      // }),
    ],

    // Since v11 the SDK collects cookies, HTTP headers/bodies and GraphQL
    // documents/variables by default. Console requests carry admin secrets,
    // JWTs and user data, so keep collection to what Sentry v7 sent.
    dataCollection: {
      // Lets Sentry infer the user's IP (see `ip_address` in `setUser` below).
      userInfo: true,
      cookies: false,
      httpHeaders: {
        request: { allow: ['User-Agent', 'Referer'] },
        response: false,
      },
      httpBodies: [],
      graphQL: { document: false, variables: false },
    },

    // Allow grouping logs by environment
    environment,
    release: tags.serverVersion,
    initialScope: {
      tags,
    },
    beforeSend(event, hint) {
      const blockError = errorMustBeBlocked({
        error: hint.originalException,
        urlPrefix: envVars.urlPrefix,
        pathname: window.location.pathname,
      });

      if (blockError) return null;

      return event;
    },
  });

  Sentry.setUser({
    id: envVars.userId,
    ip_address: '{{auto}}',
  });

  return 'enabled';
}
