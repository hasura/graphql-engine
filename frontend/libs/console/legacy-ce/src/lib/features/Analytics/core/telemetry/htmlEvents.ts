/**
 * This file contains the core Analytics utils for reusing this Analytics module for telemetry
 * It uses the common attributes exposed by the Analytics component and useGetAnalyticsAttributes hook and sets up events listeners to
 * track events that have the analytics attributes. Currently, telemetry is only set up for ee-lite, but it can be extended to CE later.
 */

import * as React from 'react';
// import { sendTelemetryEvent, HTMLUserEvent } from '../../../../telemetry'
import { DATA_ANALYTICS_ATTRIBUTE } from '../getAnalyticsAttributes';

type UserEvent = 'click' | 'change';
export type UserEventTracker = (id: string, kind: UserEvent) => void;

// This function accepts the event identifier, constructs the telemetry payload and sends it

const trackEvent = (
  target: HTMLElement,
  kind: UserEvent,
  tracker: UserEventTracker
) => {
  // if the target or one of its ancestors has the data-analytics attribute, track the event with the attribute value and event kind
  if (target && `closest` in target) {
    const matchingTarget = target.closest(`[${DATA_ANALYTICS_ATTRIBUTE}]`);
    const analyticsAttributeValue = matchingTarget?.getAttribute
      ? matchingTarget.getAttribute(DATA_ANALYTICS_ATTRIBUTE)
      : null;
    if (analyticsAttributeValue) {
      tracker(analyticsAttributeValue, kind);
    }
  }
};

const generateOnClickHandler =
  (tracker: UserEventTracker) => (e: MouseEvent) => {
    const eventTarget = e.target;
    if (eventTarget) {
      trackEvent(eventTarget as HTMLElement, 'click', tracker);
    }
  };

const generateOnChangeHandler = (tracker: UserEventTracker) => (e: Event) => {
  const eventTarget = e.target;
  if (eventTarget) {
    trackEvent(eventTarget as HTMLElement, 'change', tracker);
  }
};

// this function sets up event listeners on the document so that click/change events can be filtered and sent as telemetry events
// it also returns a cleaner so that the event listeners can be removed whenever needed
// the tracker is parameterised so
// a. the telemetry target can be changed if need be
// b. to avoid the depedency loop between telemetr <> Analytics
// c. to make the code testable
export const setupTelemetryEventListeners = (tracker: UserEventTracker) => {
  const handleOnClick = generateOnClickHandler(tracker);
  const handleOnChange = generateOnChangeHandler(tracker);
  document.addEventListener('click', handleOnClick);
  document.addEventListener('change', handleOnChange);
  return () => {
    document.removeEventListener('click', handleOnClick);
    document.removeEventListener('change', handleOnChange);
  };
};

// a hook that sets up telemetry event listeners on component mount
export const useSetupTelemetryEventListeners = (
  tracker: UserEventTracker,
  skip: boolean
) => {
  React.useEffect(() => {
    if (!skip) {
      const cleaner = setupTelemetryEventListeners(tracker);
      return cleaner;
    }
    return () => null;
  }, [skip, tracker]);
};
