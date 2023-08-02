import * as React from 'react';
import clsx from 'clsx';

import { Button } from '../../../../../new-components/Button';
import { useConsoleForm, InputField } from '../../../../../new-components/Form';
import { RequestHeadersSelector } from '../../../../../new-components/RequestHeadersSelector';

import type { FormValues } from './schema';
import { formSchema } from './schema';
import { Toggle } from './components/Toggle';
import { useResetDefaultFormValues } from './hooks/useResetDefaultFormValues';
import { CollapsibleFieldWrapper } from './components/CollapsibleFieldWrapper';
import { z } from 'zod';
import { Switch } from '../../../../../new-components/Switch';

interface FormProps {
  skeletonMode: boolean;
  firstTimeSetup: boolean;
  defaultValues: FormValues;
  onSubmit: (formValues: FormValues) => void;
  loading?: boolean;
}

/**
 * The form to update the OpenTelemetry configuration.
 */
export function Form(props: FormProps) {
  const { onSubmit, defaultValues, firstTimeSetup, skeletonMode, loading } =
    props;

  const {
    Form: ConsoleForm,
    methods: { reset, watch, setValue },
  } = useConsoleForm({
    schema: formSchema,
    options: { defaultValues },
  });

  useResetDefaultFormValues({ defaultValues, skeletonMode, reset });

  const dataType: z.infer<typeof formSchema>['dataType'] = watch('dataType');

  const traceType = dataType.includes('traces');
  const metricsType = dataType.includes('metrics');

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
      <div className="flex">
        <InputField
          name="tracesEndpoint"
          label="Traces Endpoint"
          placeholder="Your OpenTelemetry traces endpoint"
          tooltip="OpenTelemetry-compliant traces receiver endpoint URL(At the moment, only HTTP is supported). This usually ends in /v1/traces"
          learnMoreLink="https://hasura.io/docs/latest/enterprise/opentelemetry/#endpoint"
          clearButton
          loading={skeletonMode}
          className="pr-4"
          disabled={!traceType}
          prependLabel={
            <Switch
              checked={traceType}
              onCheckedChange={checked => {
                setValue(
                  'dataType',
                  checked
                    ? dataType.concat('traces')
                    : dataType.filter(type => type !== 'traces')
                );
              }}
            />
          }
        />
        <InputField
          name="metricsEndpoint"
          label="Metrics Endpoint"
          placeholder="Your OpenTelemetry metrics endpoint"
          tooltip="OpenTelemetry-compliant metrics receiver endpoint URL(At the moment, only HTTP is supported). This usually ends in /v1/metrics. Metrics will be sampled and exported every 15 seconds."
          learnMoreLink="https://hasura.io/docs/latest/enterprise/opentelemetry/#endpoint"
          clearButton
          loading={skeletonMode}
          disabled={!metricsType}
          prependLabel={
            <Switch
              checked={metricsType}
              onCheckedChange={checked => {
                setValue(
                  'dataType',
                  checked
                    ? dataType.concat('metrics')
                    : dataType.filter(type => type !== 'metrics')
                );
              }}
            />
          }
        />
      </div>
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
        isLoading={loading}
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
