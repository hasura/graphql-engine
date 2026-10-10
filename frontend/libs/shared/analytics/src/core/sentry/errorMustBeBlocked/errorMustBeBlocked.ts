import type { EventHint } from '@sentry/react';
import { isGraphiQlError } from './isGraphiQlError';

export interface ErrorInfo {
  error: EventHint['originalException'];
  pathname: string; // window.location.pathname
  urlPrefix: string; // globals.urlPrefix
}

/**
 * Identify the errors that must be blocked from hitting Sentry.
 *
 * Please note that we must be extremely careful when blocking errors, false positives are better
 * then not being notified at all!
 */
export function errorMustBeBlocked(info: ErrorInfo) {
  if (isGraphiQlError(info)) return true;

  return false;
}
