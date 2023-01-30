import * as React from 'react';

import { OpenTelemetry } from '../OpenTelemetry/OpenTelemetry';

import { useOpenTelemetry } from './hooks/useOpenTelemetry';
import { useSetOpenTelemetry } from './hooks/useSetOpenTelemetry';

/**
 * Allow isolating OpenTelemetry (the UI core of the feature) from every ap logic like
 * notifications, metadata loading, etc.
 */
export function OpenTelemetryProvider() {
  const { isLoadingMetadata, metadataFormValues, isFirstTimeSetup } =
    useOpenTelemetry();

  const { setOpenTelemetry } = useSetOpenTelemetry();

  return (
    <OpenTelemetry
      isFirstTimeSetup={isFirstTimeSetup}
      skeletonMode={isLoadingMetadata}
      metadataFormValues={metadataFormValues}
      setOpenTelemetry={setOpenTelemetry}
    />
  );
}
