import * as React from 'react';
import clsx from 'clsx';
import { useFormState } from 'react-hook-form';

import { Button } from '../../../../../new-components/Button';
import {
  CheckboxesField,
  useConsoleForm,
  InputField,
  Radio,
} from '../../../../../new-components/Form';
import { RequestHeadersSelector } from '../../../../../new-components/RequestHeadersSelector';

import type { FormValues } from './schema';
import { formSchema } from './schema';
import { Toggle } from './components/Toggle';
import { useResetDefaultFormValues } from './hooks/useResetDefaultFormValues';
import { CollapsibleFieldWrapper } from './components/CollapsibleFieldWrapper';
import { z } from 'zod';

interface FormProps {
  skeletonMode: boolean;
  firstTimeSetup: boolean;
  defaultValues: FormValues;
  onSubmit: (formValues: FormValues) => void;
}

/**
 * The form to update the OpenTelemetry configuration.
 */
export function Form(props: FormProps) {
  const { onSubmit, defaultValues, firstTimeSetup, skeletonMode } = props;

  const {
    Form: ConsoleForm,
    methods: { control, reset },
  } = useConsoleForm({
    schema: formSchema,
    options: { defaultValues },
  });

  useResetDefaultFormValues({ defaultValues, skeletonMode, reset });

  const { isSubmitting } = useFormState({ control });
  const buttonTexts = firstTimeSetup
    ? { text: 'Connect', loadingText: 'Connecting...' }
    : { text: 'Update', loadingText: 'Updating...' };

  return (
    <ConsoleForm
      onSubmit={(data: z.infer<typeof formSchema>) => {
        onSubmit(data);
      }}
    >
      <Toggle
        name="enabled"
        label="Status"
        writtenStatus={{ true: 'Enabled', false: 'Disabled' }}
        loading={skeletonMode}
      />
      {/* No need to redact the input fields since Heap avoid recording the input field values by default */}
      <InputField
        name="endpoint"
        label="Endpoint"
        placeholder="Your OpenTelemetry endpoint"
        tooltip="OpenTelemetry compliant receiver endpoint URL."
        learnMoreLink="https://hasura.io/docs/latest/enterprise/opentelemetry/#endpoint"
        clearButton
        loading={skeletonMode}
      />
      <Radio
        name="connectionType"
        label="Connection Type"
        tooltip="The protocol to be used for the communication with the receiver. At the moment, only HTTP is supported."
        learnMoreLink="https://hasura.io/docs/latest/enterprise/opentelemetry/#connection-type"
        options={[{ value: 'http/protobuf', label: 'HTTP' }]}
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
        learnMoreLink="https://hasura.io/docs/latest/enterprise/opentelemetry/#data-type"
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
        learnMoreLink="https://hasura.io/docs/latest/enterprise/opentelemetry/#batch-size"
        clearButton
        loading={skeletonMode}
      />
      <CollapsibleFieldWrapper
        inputFieldName="headers"
        label="Headers"
        tooltip="Additional custom headers added to export request."
        learnMoreLink="https://hasura.io/docs/latest/enterprise/opentelemetry/#headers"
        loading={skeletonMode}
      >
        {/* No need to redact the input fields since Heap avoid recording the input field values by default */}
        <RequestHeadersSelector name="headers" addButtonText="Add Headers" />
      </CollapsibleFieldWrapper>
      <CollapsibleFieldWrapper
        inputFieldName="attributes"
        label="Attributes"
        tooltip="Additional custom tags added to export request."
        learnMoreLink="https://hasura.io/docs/latest/enterprise/opentelemetry/#attributes"
        loading={skeletonMode}
      >
        {/* No need to redact the input fields since Heap avoid recording the input field values by default */}
        <RequestHeadersSelector
          name="attributes"
          addButtonText="Add Attributes"
          typeSelect={false}
        />
      </CollapsibleFieldWrapper>
      <Button
        type="submit"
        mode="primary"
        loadingText={buttonTexts.loadingText}
        isLoading={isSubmitting}
        // Skeleton mode
        disabled={skeletonMode}
        // Necessary to separate the button from the above skeleton
        className={clsx({ 'mt-1': skeletonMode })}
      >
        {buttonTexts.text}
      </Button>
    </ConsoleForm>
  );
}
