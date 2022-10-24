import * as Sentry from '@sentry/react';
import type { ReservedEventNames } from './heap/types';

interface CustomEventOptions {
  severity?: 'log' | 'warning' | 'error';

  /* A rich description of the event */
  message?: string;
  /* Any additional context for the event */
  data?: Record<string, string>;
}

const defaultOptions: CustomEventOptions = {
  severity: 'log',
};

/**
 * Track a custom event both in Heap and Sentry.
 */
export function trackCustomEvent<EVENT_NAME extends string>(
  name: EVENT_NAME extends ReservedEventNames ? never : EVENT_NAME,
  options?: CustomEventOptions
) {
  const trackOptions = { ...defaultOptions, ...options };

  const { message, severity } = trackOptions;
  const data = message ? { message, ...trackOptions.data } : trackOptions.data;

  // --------------------------------------------------
  // HEAP TRACKING
  // --------------------------------------------------
  switch (severity) {
    case 'error':
      const errorName = `(ERROR) ${name}`;
      window.heap?.track(errorName, data);
      break;

    case 'warning':
      const warningName = `(WARN) ${name}`;
      window.heap?.track(warningName, data);
      break;

    case 'log':
    default:
      window.heap?.track(name, data);
      break;
  }

  // --------------------------------------------------
  // SENTRY TRACKING
  // --------------------------------------------------
  Sentry.addBreadcrumb({
    message,
    type: name,
    data: options?.data,
    level: options?.severity,
  });
}
