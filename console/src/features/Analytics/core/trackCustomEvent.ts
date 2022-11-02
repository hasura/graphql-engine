import * as Sentry from '@sentry/react';

// Heap suggest using the "Location – Action – Object" pattern
// @see: https://help.heap.io/definitions/events/events-overview/#:~:text=Custom%20events%20are%20events%20that,events%20to%20keep%20them%20organized.
interface HeapEvent {
  location: string;
  action: string;
  object: string;
}

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
export function trackCustomEvent(
  event: HeapEvent,
  options?: CustomEventOptions
) {
  const eventName = `${event.location} - ${event.action} - ${event.object}`;

  const trackOptions = { ...defaultOptions, ...options };
  const { message } = trackOptions;
  const data = message ? { message, ...trackOptions.data } : trackOptions.data;

  // --------------------------------------------------
  // HEAP TRACKING
  // --------------------------------------------------
  window.heap?.track(eventName, data);

  // --------------------------------------------------
  // SENTRY TRACKING
  // --------------------------------------------------
  Sentry.addBreadcrumb({
    message,
    type: eventName,
    data: options?.data,
    level: options?.severity,
  });
}
