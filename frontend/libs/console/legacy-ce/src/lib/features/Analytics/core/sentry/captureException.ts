import * as Sentry from '@sentry/react';

/**
 * A simple wrapper around Sentry's captureException.
 *
 * @see https://docs.sentry.io/platforms/javascript/enriching-events/context/
 */
export function captureException(
  error: Error,
  level: 'error' | 'warning' = 'error'
) {
  Sentry.captureException(error, {
    level,
  });
}
