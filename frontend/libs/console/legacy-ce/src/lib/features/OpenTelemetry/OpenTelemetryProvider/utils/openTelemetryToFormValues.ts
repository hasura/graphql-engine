import type { Metadata, OpenTelemetry } from '@/features/hasura-metadata-types';
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

  if (openTelemetry.status === 'disabled') {
    return {
      enabled: false,
      endpoint: openTelemetry.exporter_otlp.otlp_traces_endpoint ?? '',
      headers: metadataHeadersToFormHeaders(
        openTelemetry.exporter_otlp.headers
      ),
      batchSize: openTelemetry.batch_span_processor.max_export_batch_size,

      attributes: metadataAttributesToFormAttributes(
        openTelemetry.exporter_otlp.resource_attributes
      ),

      // At the beginning, only one Data Type is available
      dataType: ['traces'],
      // At the beginning, only one Connection Type is available
      connectionType: 'http/protobuf',
    };
  }

  return {
    enabled: true,
    endpoint: openTelemetry.exporter_otlp.otlp_traces_endpoint,
    headers: metadataHeadersToFormHeaders(openTelemetry.exporter_otlp.headers),
    batchSize: openTelemetry.batch_span_processor.max_export_batch_size,

    attributes: metadataAttributesToFormAttributes(
      openTelemetry.exporter_otlp.resource_attributes
    ),

    // At the beginning, only one Data Type is available
    dataType: ['traces'],
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
  const otlp_traces_endpoint = formValues.endpoint;
  const max_export_batch_size = formValues.batchSize;

  // At the beginning, only one Connection Type is available
  const protocol = 'http/protobuf';

  const headers = formHeadersToMetadataHeaders(formValues.headers);
  const resource_attributes = formAttributesToMetadataAttributes(
    formValues.attributes
  );

  if (!formValues.enabled) {
    const disabledOpenTelemetry: OpenTelemetry = {
      status: 'disabled',

      // At the beginning, only one Data Type is available
      data_types: ['traces'],

      exporter_otlp: {
        headers,
        protocol,
        resource_attributes,
      },

      batch_span_processor: {
        max_export_batch_size,
      },
    };

    if (otlp_traces_endpoint) {
      disabledOpenTelemetry.exporter_otlp.otlp_traces_endpoint =
        otlp_traces_endpoint;
    }
    return disabledOpenTelemetry;
  }

  return {
    status: 'enabled',

    // At the beginning, only one Data Type is available
    data_types: ['traces'],

    exporter_otlp: {
      headers,
      protocol,
      resource_attributes,
      otlp_traces_endpoint,
    },

    batch_span_processor: {
      max_export_batch_size,
    },
  };
}
