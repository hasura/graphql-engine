import * as React from 'react';

import type { FormValues } from './components/Form/schema';
import { defaultValues } from './components/Form/schema';

import { Form } from './components/Form/Form';
import { Header } from './components/Header/Header';

interface OpenTelemetryConfigProps {
  skeletonMode: boolean;
  metadataFormValues: FormValues | undefined;
  updateOpenTelemetry: (formValues: FormValues) => Promise<void>;
}

/**
 * All the OpenTelemetry page without any external dependency.
 */
export function OpenTelemetryConfig(props: OpenTelemetryConfigProps) {
  const { skeletonMode, metadataFormValues, updateOpenTelemetry } = props;

  const formValues = metadataFormValues || defaultValues;

  const headerMode = skeletonMode
    ? 'skeleton'
    : !formValues.enabled
    ? 'disabled'
    : 'enabled';

  return (
    <div className="space-y-md max-w-screen-md p-md">
      <Header mode={headerMode} />

      <div>
        {/* This div avoid space-y-md separating too much the toggle and the form */}
        <Form
          onSubmit={updateOpenTelemetry}
          defaultValues={formValues}
          skeletonMode={skeletonMode}
        />
      </div>
    </div>
  );
}
