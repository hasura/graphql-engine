import { startTracing } from './core/startTracing';
import { parseSentryDsn } from './core/parseSentryDsn';

/**
 * A sentry object that attempts to mirror the actual sentry API.
 */
export const sentry = {
  startTracing,
  parseSentryDsn,
};
