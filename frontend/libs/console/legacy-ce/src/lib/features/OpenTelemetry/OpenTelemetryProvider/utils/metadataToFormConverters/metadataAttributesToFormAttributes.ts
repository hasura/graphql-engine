import type { OpenTelemetry } from '../../../../hasura-metadata-types';
import type { FormValues } from '../../../OpenTelemetry/components/Form/schema';

type MetadataAttributes = OpenTelemetry['exporter_otlp']['resource_attributes'];
type FormAttributes = FormValues['attributes'];

/**
 * Convert the OpenTelemetry attributes into the corresponding form values.
 */
export function metadataAttributesToFormAttributes(
  metadataAttributes: MetadataAttributes
) {
  return metadataAttributes.reduce<FormAttributes>((acc, metadataAttribute) => {
    acc.push({
      name: metadataAttribute.name,
      value: metadataAttribute.value,
      type: 'from_value',
    });

    return acc;
  }, []);
}

/**
 * Convert the form attributes into the corresponding metadata values.
 */
export function formAttributesToMetadataAttributes(
  formAttributes: FormAttributes
) {
  return formAttributes.reduce<MetadataAttributes>((acc, formAttribute) => {
    if (formAttribute.name === '') return acc;

    acc.push({
      name: formAttribute.name,
      value: formAttribute.value,
    });

    return acc;
  }, []);
}
