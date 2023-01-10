import * as React from 'react';

import { Button } from '@/new-components/Button';
import { Checkbox, SimpleForm, InputField, Radio } from '@/new-components/Form';
import { RequestHeadersSelector } from '@/new-components/RequestHeadersSelector';

import type { FormValues } from './schema';
import { FormSchema } from './schema';
import { CollapsibleFieldWrapper } from './components/CollapsibleFieldWrapper';

interface FormProps {
  loading: boolean;
  defaultValues: FormValues;
  onSubmit: (formValues: FormValues) => void;
}

export function Form(props: FormProps) {
  const { onSubmit, defaultValues, loading } = props;

  return (
    <SimpleForm
      schema={FormSchema}
      onSubmit={onSubmit}
      options={{ defaultValues }}
    >
      <>
        <InputField
          name="endpoint"
          label="Endpoint"
          placeholder="Your OpenTelemetry endpoint"
          tooltip="OpenTelemetry compliant receiver endpoint URL."
          clearButton
        />

        <Radio
          name="connectionType"
          label="Connection Type"
          tooltip="The protocol to be used for the communication with the receiver. At the moment, only HTTP is supported."
          options={[{ value: 'http', label: 'HTTP' }]}
          orientation="horizontal"
          // At the beginning, only one Connection Type is available, hence it does not make sense
          // to enable the users to change it.
          disabled
        />

        <Checkbox
          name="dataType"
          label="Data Type"
          tooltip="The type of observability data points to be exported. At the moment, only Traces is supported."
          options={[{ value: 'traces', label: 'Traces' }]}
          // At the beginning, only one Data Type ia available, hence it does not make sense
          // to enable the users to change it.
          disabled
        />

        <InputField
          name="batchSize"
          type="number"
          label="Batch Size"
          placeholder="A number between 1 and 512"
          tooltip="The maximum number of data points in an export request. The value should be between 1-512. Default value is 512."
          clearButton
        />

        <CollapsibleFieldWrapper
          inputFieldName="headers"
          label="Headers"
          tooltip="Additional custom headers added to export request."
        >
          <RequestHeadersSelector name="headers" addButtonText="Add Headers" />
        </CollapsibleFieldWrapper>

        <CollapsibleFieldWrapper
          inputFieldName="attributes"
          label="Attributes"
          tooltip="Additional custom tags added to export request."
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
          isLoading={loading}
          loadingText="Updating..."
        >
          Update
        </Button>
      </>
    </SimpleForm>
  );
}
