import * as React from 'react';
import clsx from 'clsx';
import { useFormState } from 'react-hook-form';

import { Button } from '@/new-components/Button';
import {
  CheckboxesField,
  useConsoleForm,
  InputField,
  Radio,
} from '@/new-components/Form';
import { RequestHeadersSelector } from '@/new-components/RequestHeadersSelector';

import type { FormValues } from './schema';

import { formSchema } from './schema';
import { Toggle } from './components/Toggle';
import { CollapsibleFieldWrapper } from './components/CollapsibleFieldWrapper';

interface FormProps {
  skeletonMode: boolean;
  defaultValues: FormValues;
  onSubmit: (formValues: FormValues) => void;
}

/**
 * The form to update the OpenTelemetry configuration.
 */
export function Form(props: FormProps) {
  const { onSubmit, defaultValues, skeletonMode } = props;

  const {
    Form: ConsoleForm,
    methods: { control },
  } = useConsoleForm({
    schema: formSchema,
    options: { defaultValues },
  });

  const { isSubmitting } = useFormState({ control });

  return (
    <ConsoleForm onSubmit={onSubmit}>
      <Toggle
        name="enabled"
        label="Status"
        writtenStatus={{ true: 'Enabled', false: 'Disabled' }}
        loading={skeletonMode}
      />

      <InputField
        name="endpoint"
        label="Endpoint"
        placeholder="Your OpenTelemetry endpoint"
        tooltip="OpenTelemetry compliant receiver endpoint URL."
        clearButton
        loading={skeletonMode}
      />

      <Radio
        name="connectionType"
        label="Connection Type"
        tooltip="The protocol to be used for the communication with the receiver. At the moment, only HTTP is supported."
        options={[{ value: 'http', label: 'HTTP' }]}
        loading={skeletonMode}
        // At the beginning, only one Connection Type is available, hence it does not make sense
        // to enable the users to change it.
        // TODO: replace it with readonly when the input fields support it
        disabled
      />

      <CheckboxesField
        name="dataType"
        label="Data Type"
        tooltip="The type of observability data points to be exported. At the moment, only Traces is supported."
        options={[{ value: 'traces', label: 'Traces' }]}
        loading={skeletonMode}
        // At the beginning, only one Data Type ia available, hence it does not make sense
        // to enable the users to change it.
        // TODO: replace it with readonly when the input fields support it
        disabled
      />

      <InputField
        name="batchSize"
        type="number"
        label="Batch Size"
        placeholder="A number between 1 and 512"
        tooltip="The maximum number of data points in an export request. The value should be between 1-512. Default value is 512."
        clearButton
        loading={skeletonMode}
      />

      <CollapsibleFieldWrapper
        inputFieldName="headers"
        label="Headers"
        tooltip="Additional custom headers added to export request."
        loading={skeletonMode}
      >
        <RequestHeadersSelector name="headers" addButtonText="Add Headers" />
      </CollapsibleFieldWrapper>

      <CollapsibleFieldWrapper
        inputFieldName="attributes"
        label="Attributes"
        tooltip="Additional custom tags added to export request."
        loading={skeletonMode}
      >
        <RequestHeadersSelector
          name="attributes"
          addButtonText="Add Attributes"
          typeSelect={false}
        />
      </CollapsibleFieldWrapper>

      <Button
        type="submit"
        mode="primary"
        loadingText="Updating..."
        isLoading={isSubmitting}
        // Skeleton mode
        disabled={skeletonMode}
        // Necessary to separate the button from the above skeleton
        className={clsx({ 'mt-1': skeletonMode })}
      >
        Update
      </Button>
    </ConsoleForm>
  );
}
