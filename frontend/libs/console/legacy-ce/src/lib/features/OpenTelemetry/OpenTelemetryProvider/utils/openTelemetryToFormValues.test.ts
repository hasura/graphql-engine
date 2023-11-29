import type { Metadata } from '../../../hasura-metadata-types';
import type { FormValues } from '../../OpenTelemetry/components/Form/schema';

import {
  formValuesToOpenTelemetry,
  openTelemetryToFormValues,
} from './openTelemetryToFormValues';

describe('openTelemetryToFormValues', () => {
  const openTelemetry: Metadata['metadata']['opentelemetry'] = {
    status: 'disabled',

    exporter_otlp: {
      resource_attributes: [],
      protocol: 'http/protobuf',
      headers: [{ name: 'baz', value: 'qux' }],
      otlp_traces_endpoint: 'https://hasura.io/v1/traces',
      otlp_metrics_endpoint: 'https://hasura.io/v1/metrics',
      otlp_logs_endpoint: 'https://hasura.io/v1/logs',
    },

    data_types: ['traces', 'metrics', 'logs'],
    batch_span_processor: {
      max_export_batch_size: 100,
    },
  };

  const formValues: FormValues = {
    enabled: false,

    batchSize: 100,
    attributes: [],
    logsEndpoint: 'https://hasura.io/v1/logs',
    tracesEndpoint: 'https://hasura.io/v1/traces',
    metricsEndpoint: 'https://hasura.io/v1/metrics',
    headers: [{ name: 'baz', value: 'qux', type: 'from_value' }],

    dataType: ['traces', 'metrics', 'logs'],
    // At the beginning, only one Connection Type is available
    connectionType: 'http/protobuf',
  };

  it('When passed with a OpenTelemetry, should return the same values for the form', () => {
    expect(openTelemetryToFormValues(openTelemetry)).toEqual(formValues);
  });

  it('When passed with some form values, should return the same values for the OpenTelemetry', () => {
    expect(formValuesToOpenTelemetry(formValues)).toEqual(openTelemetry);
  });

  it('When passed with a disabled configuration and an empty endpoint, should strip out the endpoint from the OpenTelemetry that must be sent to the server', () => {
    expect(
      formValuesToOpenTelemetry({
        enabled: false,

        batchSize: 100,
        attributes: [],
        tracesEndpoint: '',
        metricsEndpoint: '',
        logsEndpoint: '',
        headers: [{ name: 'baz', value: 'qux', type: 'from_value' }],

        // At the beginning, only one Data Type is available
        dataType: ['traces'],
        // At the beginning, only one Connection Type is available
        connectionType: 'http/protobuf',
      })
    ).toEqual({
      status: 'disabled',

      exporter_otlp: {
        resource_attributes: [],
        protocol: 'http/protobuf',
        headers: [{ name: 'baz', value: 'qux' }],
      },

      data_types: ['traces'],
      batch_span_processor: {
        max_export_batch_size: 100,
      },
    });
  });
});
