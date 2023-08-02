import type { Metadata } from '../../../hasura-metadata-types';

import { parseOpenTelemetry } from '../../../hasura-metadata-types';
import {
  programmaticallyTraceError,
  trackCustomEvent,
} from '../../../Analytics';

/**
 * Parse the OpenTelemetry config stored in metadata. There are two possibilities where server's
 * metadata is not aligned with the Console one:
 * 1. (NOT CAUGHT) When the server adds more data than the expected ones. In this case, the Console
 * for sure will lose the extra data because the immutability nature of the Console brings to never
 * work with the original object. Even if catching this error would be a nice to have, it's not easy
 * to implement since zod's passThrough is not easy to be used along the whole data-management chain.
 * Zod's strict(), instead, is not useable with discriminated unions.
 * 2. (CAUGHT) When the server removes/changes some data. In this case, the Console reports the error
 * in Sentry.
 *
 * Theoretically speaking, every misalignment between the server and the Console is a huge problem
 * (especially if the server object contains less properties than what the Console expects) and
 * breaking the Console through an error could be the best thing to do (instead of using
 * programmaticallyTraceError). Anyway, most of the misalignments could be related to the validation
 * of the single properties (ex. the endpoint being `null` instead an empty string). In case of these
 * soft misalignments, the Console could continue to work as expected, hence it does not makes sense
 * to break it. The Console folks will notice the error in Sentry.
 *
 * PLEASE NOTE: The problem this function tries to work around is a pure communication problem that
 * happens only if the server and the Console are not aligned. When the TypeScript types coming from
 * the OpenAPI specs will be there, we do not need this function anymore.
 *
 * PEASE NOTE: Validating the metadata should happen in a centralized manner, not in every single
 * feature... Because, at the moment, the OpenTelemetry part never gets validated until the users
 * navigates to the OpenTelemetry page.
 */
export function useTrackTypeMisalignments(
  openTelemetry: Metadata['metadata']['opentelemetry']
) {
  // metadata.opentelemetry is not there if the users never set it up
  if (openTelemetry === undefined) return;

  const result = parseOpenTelemetry(openTelemetry);

  if (result.success) return;

  trackCustomEvent(
    {
      location: 'OpenTelemetry',
      action: 'parse',
      object: 'OpenTelemetry parser',
    },
    {
      severity: 'error',
      message: result.error.message,
      data: {
        openTelemetry: JSON.stringify(openTelemetry),
      },
    }
  );

  programmaticallyTraceError({
    error: 'OpenTelemetry metadata not parsed',
    cause: result.error,
  });
}
