import * as Sentry from '@sentry/react';

export function isSentryAlreadyStarted() {
  const tracingAlreadyStarted = !!Sentry.getClient();

  return tracingAlreadyStarted;
}
