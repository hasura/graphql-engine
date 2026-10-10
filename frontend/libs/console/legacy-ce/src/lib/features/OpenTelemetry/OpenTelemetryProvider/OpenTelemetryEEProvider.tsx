import * as React from 'react';

import { OpenTelemetry } from '../OpenTelemetry/OpenTelemetry';

import { useOpenTelemetry } from './hooks/useOpenTelemetry';
import { useSetOpenTelemetry } from './hooks/useSetOpenTelemetry';
import { useEELiteAccess } from '../../EETrial';

/**
 * Allow isolating OpenTelemetry (the UI core of the feature) from every ap logic like
 * notifications, metadata loading, etc.
 */
export function OpenTelemetryEEProvider() {
  const { access } = useEELiteAccess();
  const { isLoadingMetadata, metadataFormValues, isFirstTimeSetup } =
    useOpenTelemetry();

  const { setOpenTelemetry, isPending } = useSetOpenTelemetry();

  return (
    <OpenTelemetry
      isFirstTimeSetup={isFirstTimeSetup}
      skeletonMode={isLoadingMetadata || access === 'loading'}
      metadataFormValues={metadataFormValues}
      setOpenTelemetry={setOpenTelemetry}
      withoutLicense={access !== 'active'}
      eeAccess={access}
      loading={isPending}
    />
  );
}
