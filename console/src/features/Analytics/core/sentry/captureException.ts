import * as Sentry from '@sentry/react';

export type ExceptionContext = {
  sourceError?: Error;
  errorMessage?: string;
};

/**
 * This function allows us to capture caught exceptions that we want the engineering team to be
 * alerted about
 *
 * @see https://docs.sentry.io/platforms/javascript/enriching-events/context/
 */
export function captureException(
  error: Error,
  exceptionContext: ExceptionContext = {},
  level: 'error' | 'warning' = 'error'
) {
  Sentry.captureException(error, {
    level,
    contexts: { debug: exceptionContext },
  });
}
