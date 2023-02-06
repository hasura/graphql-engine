import * as Sentry from '@sentry/react';
import { captureException } from './sentry/captureException';

type HumanReadableString = string;

type Options = {
  level?: 'error' | 'warning';
  /**
   * Logging to the Console has a specific purpose: Sentry is NOT enabled for every type of Console
   * (at the time of writing, Sentry is enabled only for the Cloud Console). Logging to the browser's
   * console allows the eventual customers that is trying to understand the root cause of a problem,
   * to report what they see in the browser's console in the issue they are going to open.
   */
  logToConsole?: boolean;
};

type HumanReadableStringWithErrorCauseAndOptions = Options & {
  error: HumanReadableString;
  cause?: Error;
};
type ErrorWithOptions = Options & {
  error: Error;
};

const noop = () => {};

/**
 * Programmatically trace a caught error.
 *
 * @example <caption>Simplest usage: pass just a string.</caption>
 * programmaticallyTraceError('Something went wrong when updating the metadata')
 *
 * @example <caption>Simplest usage: pass the error you receive.</caption>
 * catch (error) {
 *   programmaticallyTraceError(error)
 * }
 *
 * @example <caption>Pass a human-readable message, but also the causing error.</caption>
 * catch (error) {
 *   programmaticallyTraceError({ error: 'Something went wrong when updating the metadata', cause: error })
 * }
 */
export function programmaticallyTraceError(
  errorOrErrorWithOptions:
    | HumanReadableString
    | Error
    | HumanReadableStringWithErrorCauseAndOptions
    | ErrorWithOptions
) {
  // --------------------------------------------------
  // SIMPLE USAGE
  // --------------------------------------------------

  // If you pass a string, it's converted to an error and passed to Sentry.
  if (typeof errorOrErrorWithOptions === 'string') {
    captureException(new Error(errorOrErrorWithOptions));

    return;
  }

  // If you pass an error, it's passed to Sentry as is.
  if (errorOrErrorWithOptions instanceof Error) {
    captureException(errorOrErrorWithOptions);

    return;
  }

  // --------------------------------------------------
  // OPTIONS-RICH USAGE
  // --------------------------------------------------

  const {
    error,
    level = 'error',
    logToConsole = true,
  } = errorOrErrorWithOptions;

  const consoleLog = logToConsole
    ? level === 'warning'
      ? console.warn
      : console.error
    : noop;

  // If you pass a cause, the cause itself is the original error and IT IS the error that will be
  // tracked to Sentry. Instead, the passed human-friendly string will be added as a breadcrumb that
  // you can see in Sentry right after the error.
  if ('cause' in errorOrErrorWithOptions && !!errorOrErrorWithOptions.cause) {
    const { cause } = errorOrErrorWithOptions;

    const message = typeof error === 'string' ? error : error.message;

    Sentry.addBreadcrumb({ level, message });
    captureException(cause);
    consoleLog(message);
    consoleLog(cause);

    return;
  }

  // The string will be converted to an error and tracked in Sentry
  if (typeof error === 'string') {
    const errorToLog = new Error(error);

    captureException(errorToLog, level);
    consoleLog(errorToLog);

    return;
  }

  // The error is tracked in Sentry
  captureException(error, level);
  consoleLog(error);
}
