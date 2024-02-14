import * as React from 'react';
import {
  useSetupTelemetryEventListeners,
  UserEventTracker,
} from '../htmlEvents';

// This component exists so that the logic of the `useSetupTelemetryEventListeners` hook can be run in class components
// This is important because the entrypoint of both CE and EE console is `Main.js`, which is a class component
export const InitializeTelemetry = (props: {
  tracker: UserEventTracker;
  skip: boolean;
}) => {
  const { tracker, skip } = props;
  useSetupTelemetryEventListeners(tracker, skip);
  return null;
};
