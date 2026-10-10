import type { EnvVars } from '@hasura/shared/types';

import { startSentryTracing } from './sentry/startSentryTracing';

/**
 * Start tracing analytics idempotently.
 */
export function startTracing(envVars: EnvVars) {
  // --------------------------------------------------
  // SENTRY
  // --------------------------------------------------
  startSentryTracing(envVars);

  // --------------------------------------------------
  // HEAP
  // --------------------------------------------------
  // No need to manually start Heap to because the server controls it (see the source of a cloud
  // application) to find the Heap script
}
