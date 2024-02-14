import * as React from 'react';
import clsx from 'clsx';

import { Button } from '../../../../../new-components/Button';
import {
  useConsoleForm,
  InputField,
  CheckboxesField,
} from '../../../../../new-components/Form';
import { RequestHeadersSelector } from '../../../../../new-components/RequestHeadersSelector';

import type { FormValues } from './schema';
import { formSchema, tracesPropagatorSchema } from './schema';
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
  const logsType = dataType.includes('logs');

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
      <div>
        <InputField
          name="tracesEndpoint"
          label="Traces Endpoint"
          placeholder="Your OpenTelemetry traces endpoint"
          tooltip="OpenTelemetry-compliant traces receiver endpoint URL(At the moment, only HTTP is supported). This usually ends in /v1/traces. Environment variable templating is available using the {{VARIABLE}} tag"
          learnMoreLink="https://hasura.io/docs/latest/observability/opentelemetry/#endpoint"
          clearButton
          loading={skeletonMode}
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
          tooltip="OpenTelemetry-compliant metrics receiver endpoint URL(At the moment, only HTTP is supported). This usually ends in /v1/metrics. Metrics will be sampled and exported every 15 seconds. Environment variable templating is available using the {{VARIABLE}} tag"
          learnMoreLink="https://hasura.io/docs/latest/observability/opentelemetry/#endpoint"
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
        <InputField
          name="logsEndpoint"
          label="Logs Endpoint"
          placeholder="Your OpenTelemetry logs endpoint"
          tooltip="OpenTelemetry-compliant logs receiver endpoint URL(At the moment, only HTTP is supported). This usually ends in /v1/logs. Environment variable templating is available using the {{VARIABLE}} tag"
          learnMoreLink="https://hasura.io/docs/latest/observability/opentelemetry/#endpoint"
          clearButton
          loading={skeletonMode}
          disabled={!logsType}
          prependLabel={
            <Switch
              checked={logsType}
              onCheckedChange={checked => {
                setValue(
                  'dataType',
                  checked
                    ? dataType.concat('logs')
                    : dataType.filter(type => type !== 'logs')
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
        learnMoreLink="https://hasura.io/docs/latest/observability/opentelemetry/#batch-size"
        clearButton
        loading={skeletonMode}
      />
      <div>
        <CheckboxesField
          name="tracesPropagators"
          label="Trace Propagations"
          orientation="horizontal"
          tooltip="The specification that exchanges trace context propagation data between services and processes. The b3 propagation is enabled by default."
          learnMoreLink="https://hasura.io/docs/latest/observability/opentelemetry/#trace-propagations"
          loading={skeletonMode}
          options={tracesPropagatorSchema.options.map(option => ({
            label: option,
            value: option,
            disabled: option === 'b3',
          }))}
        />
      </div>
      <CollapsibleFieldWrapper
        inputFieldName="headers"
        label="Headers"
        tooltip="Additional custom headers added to export request."
        learnMoreLink="https://hasura.io/docs/latest/observability/opentelemetry/#headers"
        loading={skeletonMode}
      >
        {/* No need to redact the input fields since Heap avoid recording the input field values by default */}
        <RequestHeadersSelector name="headers" addButtonText="Add Headers" />
      </CollapsibleFieldWrapper>
      <CollapsibleFieldWrapper
        inputFieldName="attributes"
        label="Attributes"
        tooltip="Additional custom tags added to export request."
        learnMoreLink="https://hasura.io/docs/latest/observability/opentelemetry/#attributes"
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
