import type { OpenTelemetry } from '../../../../hasura-metadata-types';
import type { FormValues } from '../../../OpenTelemetry/components/Form/schema';

type MetadataHeaders = OpenTelemetry['exporter_otlp']['headers'];
type FormHeaders = FormValues['headers'];

/**
 * Convert the OpenTelemetry headers into the corresponding form values.
 */
export function metadataHeadersToFormHeaders(metadataHeaders: MetadataHeaders) {
  return metadataHeaders.reduce<FormHeaders>((acc, metadataHeader) => {
    // The name cannot be used as a discriminator, but the presence of 'value_from_env' can
    if ('value_from_env' in metadataHeader) {
      acc.push({
        name: metadataHeader.name,
        value: metadataHeader.value_from_env,
        type: 'from_env',
      });

      return acc;
    }

    acc.push({
      name: metadataHeader.name,
      value: metadataHeader.value,
      type: 'from_value',
    });

    return acc;
  }, []);
}

/**
 * Convert the form headers into the corresponding metadata values.
 */
export function formHeadersToMetadataHeaders(formHeaders: FormHeaders) {
  return formHeaders.reduce<MetadataHeaders>((acc, formHeader) => {
    if (formHeader.name === '') return acc;

    if (formHeader.type === 'from_env') {
      acc.push({
        name: formHeader.name,
        value_from_env: formHeader.value,
      });

      return acc;
    }

    acc.push({
      name: formHeader.name,
      value: formHeader.value,
    });

    return acc;
  }, []);
}
