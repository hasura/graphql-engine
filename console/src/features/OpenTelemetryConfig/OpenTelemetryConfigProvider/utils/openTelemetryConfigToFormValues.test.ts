import type { Metadata } from '@/features/hasura-metadata-types';
import type { FormValues } from '../../OpenTelemetryConfig/components/Form/schema';

import {
  formValuesToOpenTelemetryConfig,
  openTelemetryConfigToFormValues,
} from './openTelemetryConfigToFormValues';

describe('openTelemetryConfigToFormValues', () => {
  const openTelemetryConfig: Metadata['metadata']['opentelemetry'] = {
    status: 'disabled',

    exporter_otlp: {
      resource_attributes: [],
      protocol: 'http/protobuf',
      headers: [{ name: 'baz', value: 'qux' }],
      otlp_traces_endpoint: 'https://hasura.io',
    },

    data_types: ['traces'],
    batch_span_processor: {
      max_export_batch_size: 100,
    },
  };

  const formValues: FormValues = {
    enabled: false,

    batchSize: 100,
    attributes: [],
    endpoint: 'https://hasura.io',
    headers: [{ name: 'baz', value: 'qux', type: 'from_value' }],

    // At the beginning, only one Data Type is available
    dataType: ['traces'],
    // At the beginning, only one Connection Type is available
    connectionType: 'http/protobuf',
  };

  it('When passed with a OpenTelemetryConfig, should return the same values for the form', () => {
    expect(openTelemetryConfigToFormValues(openTelemetryConfig)).toEqual(
      formValues
    );
  });

  it('When passed with some form values, should return the same values for the OpenTelemetryConfig', () => {
    expect(formValuesToOpenTelemetryConfig(formValues)).toEqual(
      openTelemetryConfig
    );
  });
});
