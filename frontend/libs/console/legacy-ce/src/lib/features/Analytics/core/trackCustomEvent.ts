import * as Sentry from '@sentry/react';

// Heap suggest using the "Location – Action – Object" pattern
// @see: https://help.heap.io/definitions/events/events-overview/#:~:text=Custom%20events%20are%20events%20that,events%20to%20keep%20them%20organized.
interface HeapEvent {
  /**
   * Whatever helps the reader to identify where the event happened.
   * @example 'GraphiQl'
   * @example 'GraphiQl > Code Exporter'
   * @example 'Action > Response Transform'
   */
  location: string;

  /**
   * The action/event to track.
   * @example 'click'
   * @example 'toggle'
   */
  action: string;

  /**
   * The object triggering the action.
   * @example 'Prettify button'
   * @example 'OpenTelemetry enabler'
   */
  object: string;
}

interface CustomEventOptions {
  severity?: 'log' | 'warning' | 'error';

  /* A rich description of the event */
  message?: string;
  /* Any additional context for the event */
  data?: Record<string, string>;
}

/**
 * Track a custom event in Heap and breadcrumb in Sentry.
 */
export function trackCustomEvent(
  event: HeapEvent,
  options?: CustomEventOptions
) {
  const eventName = `${event.location} - ${event.action} - ${event.object}`;

  const { message, data, severity = 'log' } = options ?? {};

  // --------------------------------------------------
  // HEAP TRACKING
  // --------------------------------------------------
  const heapEventProperties: Record<string, string> = {};

  if (message) heapEventProperties.message = message;

  window.heap?.track(eventName, {
    ...heapEventProperties,
    ...data,
  });

  // --------------------------------------------------
  // SENTRY TRACKING
  // --------------------------------------------------
  Sentry.addBreadcrumb({
    type: eventName,
    level: severity,
    message,
    data,
  });
}
