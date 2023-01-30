import { z } from 'zod';

// --------------------------------------------------
// UTILS
// --------------------------------------------------
const validUrlSchema = z.string().url({ message: 'Invalid URL' });

// --------------------------------------------------
// ATTRIBUTES
// --------------------------------------------------
const attributeSchema = z.object({
  name: z.string(),

  // ATM, only string values are accepted out of the wide variety of values OpenTelemetry accepts
  // see: https://opentelemetry.io/docs/reference/specification/common/#attribute
  value: z.string(),
});

// --------------------------------------------------
// HEADERS
// --------------------------------------------------
const userDefinedHeaderSchema = z.object({
  // Names should be validated against /^[a-zA-Z0-9]*$/ but we must be sure the server performs the
  // same check.
  // see: the CloudFlare docs as an example https://developers.cloudflare.com/rules/transform/request-header-modification/reference/header-format/#:~:text=The%20value%20of%20the%20HTTP,%24%26%60%7C~%5E%25
  name: z.string(),
  // Values should be validated against /^[a-zA-Z0-9_ :;.,\\\/"'\?\!\(\)\{\}\[\]@<>=\-+\*#$&`|~^%]*$/
  // but we must be sure the server performs the same check.
  // see: the CloudFlare docs as an example https://developers.cloudflare.com/rules/transform/request-header-modification/reference/header-format/#:~:text=The%20value%20of%20the%20HTTP,%24%26%60%7C~%5E%25
  value: z.string(),
});
const envVarHeaderSchema = z.object({
  // Names should be validated against /^[a-zA-Z0-9]*$/ but we must be sure the server performs the
  // same check.
  // see: the CloudFlare docs as an example https://developers.cloudflare.com/rules/transform/request-header-modification/reference/header-format/#:~:text=The%20value%20of%20the%20HTTP,%24%26%60%7C~%5E%25
  name: z.string(),
  // FYI: The env vars do exist in the server! The server prevents setting unexisting env vars
  // Values should be validated against /^[a-zA-Z0-9_ :;.,\\\/"'\?\!\(\)\{\}\[\]@<>=\-+\*#$&`|~^%]*$/
  // but we must be sure the server performs the same check.
  // see: the CloudFlare docs as an example https://developers.cloudflare.com/rules/transform/request-header-modification/reference/header-format/#:~:text=The%20value%20of%20the%20HTTP,%24%26%60%7C~%5E%25
  value_from_env: z.string(),
});

const headersSchema = z.array(
  z.union([userDefinedHeaderSchema, envVarHeaderSchema])
);

// --------------------------------------------------
// OTHER PROPERTIES
// --------------------------------------------------

// In the future, also 'metrics' | 'logs' will be available
const dataTypesSchema = z.literal('traces');

// In the future, also 'grpc' will be available
const protocolSchema = z.literal('http/protobuf');

// --------------------------------------------------
// EXPORTER
// --------------------------------------------------

const exporterSchema = z.object({
  headers: headersSchema,
  protocol: protocolSchema,
  resource_attributes: z.array(attributeSchema),

  /**
   * The most important part of the configuration. You cannot enable OpenTelemetry without a valid
   * endpoint.
   */
  otlp_traces_endpoint: validUrlSchema,
});

// --------------------------------------------------
// ENABLED/DISABLED DIFFERENCES
// --------------------------------------------------

const enabledOpenTelemetrySchema = {
  /**
   * If OpenTelemetry is enabled or not. Allows to enable/disable the feature without losing the
   * configuration and/or inferring the status from other data (initially, data_types meant that
   * OpenTelemetry is disabled if it is empty but this is not true anymore because of the bad UX
   * consequences).
   */
  status: z.literal('enabled'),

  /**
   * The request headers sent to the OpenTelemetry endpoint.
   */
  data_types: z.array(dataTypesSchema),

  batch_span_processor: z.object({
    // a value between 1 and 512
    max_export_batch_size: z.number().min(1).max(512),
  }),

  exporter_otlp: exporterSchema,
};

const disabledOpenTelemetrySchema = {
  ...enabledOpenTelemetrySchema,

  status: z.literal('disabled'),

  exporter_otlp: exporterSchema.extend({
    // If OpenTelemetry is disabled, the endpoint is not required
    otlp_traces_endpoint: validUrlSchema
      .or(z.literal(''))
      .or(z.literal(undefined)),
  }),
};

// --------------------------------------------------
// OPEN TELEMETRY
// --------------------------------------------------
export const openTelemetrySchema = z.discriminatedUnion('status', [
  z.object(disabledOpenTelemetrySchema),
  z.object(enabledOpenTelemetrySchema),
]);

export type OpenTelemetry = z.infer<typeof openTelemetrySchema>;
