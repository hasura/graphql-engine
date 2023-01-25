import type { ExceptionContext } from './sentry/captureException';
import { captureException } from './sentry/captureException';

/**
 * Programmatically trace a caught error.
 */
export function programmaticallyTraceError(
  error: Error,
  exceptionContext: ExceptionContext = {},
  level: 'error' | 'warning' = 'error'
) {
  captureException(error, exceptionContext, level);
}
