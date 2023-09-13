import type { Metadata, OpenTelemetry } from '../../../hasura-metadata-types';
import type { FormValues } from '../../OpenTelemetry/components/Form/schema';

import { defaultValues } from '../../OpenTelemetry/components/Form/schema';

import {
  formHeadersToMetadataHeaders,
  metadataHeadersToFormHeaders,
} from './metadataToFormConverters/metadataHeadersToFormHeaders';
import {
  formAttributesToMetadataAttributes,
  metadataAttributesToFormAttributes,
} from './metadataToFormConverters/metadataAttributesToFormAttributes';

/**
 * Convert a metadata's OpenTelemetry configuration into its corresponding form values object.
 *
 * ATTENTION: It takes for granted the OpenTelemetry configuration received from the server respects
 * the type! Misalignments, if any, must be caught before calling openTelemetryToFormValues!
 */
export function openTelemetryToFormValues(
  openTelemetry: Metadata['metadata']['opentelemetry']
): FormValues {
  if (!openTelemetry) return defaultValues;

  return {
    enabled: openTelemetry.status === 'enabled',
    tracesEndpoint: openTelemetry.exporter_otlp.otlp_traces_endpoint ?? '',
    metricsEndpoint: openTelemetry.exporter_otlp.otlp_metrics_endpoint ?? '',
    logsEndpoint: openTelemetry.exporter_otlp.otlp_logs_endpoint ?? '',
    headers: metadataHeadersToFormHeaders(openTelemetry.exporter_otlp.headers),
    batchSize: openTelemetry.batch_span_processor.max_export_batch_size,
    tracesPropagators: openTelemetry.exporter_otlp.traces_propagators,
    attributes: metadataAttributesToFormAttributes(
      openTelemetry.exporter_otlp.resource_attributes
    ),

    dataType: openTelemetry.data_types,
    // At the beginning, only one Connection Type is available
    connectionType: 'http/protobuf',
  };
}

/**
 * Convert the form values their corresponding metadata object.
 */
export function formValuesToOpenTelemetry(
  formValues: FormValues
): OpenTelemetry {
  const otlp_traces_endpoint = formValues.tracesEndpoint;
  const otlp_metrics_endpoint = formValues.metricsEndpoint;
  const otlp_logs_endpoint = formValues.logsEndpoint;
  const max_export_batch_size = formValues.batchSize;
  const traces_propagators = formValues.tracesPropagators;
  // At the beginning, only one Connection Type is available
  const protocol = 'http/protobuf';

  const headers = formHeadersToMetadataHeaders(formValues.headers);
  const resource_attributes = formAttributesToMetadataAttributes(
    formValues.attributes
  );
  const status = formValues.enabled ? 'enabled' : 'disabled';
  const data_types = formValues.dataType;

  const ot: OpenTelemetry = {
    status,
    data_types,

    exporter_otlp: {
      headers,
      protocol,
      resource_attributes,
      traces_propagators,
    },

    batch_span_processor: {
      max_export_batch_size,
    },
  };

  if (otlp_traces_endpoint) {
    ot.exporter_otlp.otlp_traces_endpoint = otlp_traces_endpoint;
  }
  if (otlp_metrics_endpoint) {
    ot.exporter_otlp.otlp_metrics_endpoint = otlp_metrics_endpoint;
  }
  if (otlp_logs_endpoint) {
    ot.exporter_otlp.otlp_logs_endpoint = otlp_logs_endpoint;
  }
  return ot;
}
