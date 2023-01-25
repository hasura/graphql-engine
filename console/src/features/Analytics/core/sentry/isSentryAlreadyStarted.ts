import * as Sentry from '@sentry/react';

export function isSentryAlreadyStarted() {
  // See https://github.com/getsentry/sentry-go/issues/9#issuecomment-619615289
  const tracingAlreadyStarted = !!Sentry.getCurrentHub().getClient();

  return tracingAlreadyStarted;
}
