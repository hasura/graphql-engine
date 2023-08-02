import { useMetadata } from '../../../hasura-metadata-api';

import { openTelemetryToFormValues } from '../utils/openTelemetryToFormValues';
import { useNotifyMetadataLoadingError } from './useNotifyMetadataLoadingError';
import { useTrackTypeMisalignments } from './useTrackTypeMisalignments';

/**
 * Retrieve the OpenTelemetry configuration from the metadata.
 */
export function useOpenTelemetry() {
  const {
    data: openTelemetry,
    isLoading: isLoadingMetadata,
    isError: loadingMetadataFailed,
  } = useMetadata(metadata => metadata.metadata.opentelemetry);

  useNotifyMetadataLoadingError(loadingMetadataFailed);
  useTrackTypeMisalignments(openTelemetry);

  const metadataFormValues = openTelemetryToFormValues(openTelemetry);

  return {
    isFirstTimeSetup: !openTelemetry,
    isLoadingMetadata,
    metadataFormValues,
  };
}
