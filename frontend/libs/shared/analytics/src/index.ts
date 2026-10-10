export type { GlobalWindowHeap } from './core/heap/types';
export * from './types';

// CORE UTILITIES
export { startTracing } from './core/startTracing';
export { addUserProperties } from './core/addUserProperties';
export { parseSentryDsn } from './core/sentry/parseSentryDsn';
export { REDACT_EVERYTHING } from './core/heap/getRedactAttributes';
export { getAnalyticsAttributes } from './core/getAnalyticsAttributes';
export { programmaticallyTraceError } from './core/programmaticallyTraceError';

// REACT UTILITIES
export { Analytics } from './components/Analytics';
export { InitializeTelemetry } from './core/telemetry/components/InitializeTelemetry';
export { useGetAnalyticsAttributes } from './hooks/useGetAnalyticsAttributes';

// CUSTOM EVENTS
export { trackCustomEvent } from './core/trackCustomEvent';
