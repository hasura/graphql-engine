import type { EnvVars } from '../../../Globals';

import globals from '../../../Globals';

import { startSentryTracing } from './sentry/startSentryTracing';

type Globals = typeof globals;

/**
 * Start tracing analytics idempotently.
 */
export function startTracing(globalVars: Globals, envVars: EnvVars) {
  // --------------------------------------------------
  // SENTRY
  // --------------------------------------------------
  startSentryTracing(globalVars, envVars);

  // --------------------------------------------------
  // HEAP
  // --------------------------------------------------
  // No need to manually start Heap to because the server controls it (see the source of a cloud
  // application) to find the Heap script
}
