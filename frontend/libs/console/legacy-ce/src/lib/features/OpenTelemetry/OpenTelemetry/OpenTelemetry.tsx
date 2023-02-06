import * as React from 'react';

import type { FormValues } from './components/Form/schema';
import { defaultValues } from './components/Form/schema';

import { Form } from './components/Form/Form';
import { Header } from './components/Header/Header';

interface OpenTelemetryProps {
  skeletonMode: boolean;
  isFirstTimeSetup: boolean;
  metadataFormValues: FormValues;
  setOpenTelemetry: (formValues: FormValues) => Promise<void>;
}

/**
 * All the OpenTelemetry page visual elements.
 */
export function OpenTelemetry(props: OpenTelemetryProps) {
  const {
    skeletonMode,
    isFirstTimeSetup,
    metadataFormValues,
    setOpenTelemetry,
  } = props;

  const formValues = metadataFormValues || defaultValues;

  let headerMode: 'enabled' | 'disabled' | 'skeleton' = 'enabled';

  if (!metadataFormValues || !metadataFormValues.enabled)
    headerMode = 'disabled';

  if (skeletonMode) headerMode = 'skeleton';

  return (
    <div className="space-y-md max-w-screen-md p-md">
      {/*
        While the form is stateful and shows its own version of the OpenTelemetry config, the
        Header reflects the real OpenTelemetry config stored in the metadata. It means that when
        the users enable OpenTelemetry through the form toggle, the Header will show the "disabled"
        status because OpenTelemetry is not enabled until the users submit the form. Once submitted,
        and the metadata is invalidated and reloaded, then the Header will reflect the "enabled"
        status.
        */}
      <Header mode={headerMode} />

      <div>
        {/* This div avoid space-y-md separating too much the toggle and the form */}
        <Form
          onSubmit={setOpenTelemetry}
          defaultValues={formValues}
          skeletonMode={skeletonMode}
          firstTimeSetup={isFirstTimeSetup}
        />
      </div>
    </div>
  );
}
