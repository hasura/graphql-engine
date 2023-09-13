import { z } from 'zod';

// --------------------------------------------------
// UTILS
// --------------------------------------------------
const validUrlSchema = z.string();

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

// In the future, also 'grpc' will be available
const protocolSchema = z.literal('http/protobuf');

// --------------------------------------------------
// EXPORTER
// --------------------------------------------------

const exporterSchema = z.object({
  headers: headersSchema,
  protocol: protocolSchema,
  resource_attributes: z.array(attributeSchema),

  /*
   * Enable extra trace propagators besides b3
   */
  traces_propagators: z.array(z.enum(['b3', 'tracecontext'])),

  /**
   * The most important parts of the configuration. If OpenTelemetry export is
   * enabled globally, AND a specific telemetry type is enabled, then a valid
   * endpoint URL must be provided.
   */
  otlp_traces_endpoint: validUrlSchema,
  otlp_metrics_endpoint: validUrlSchema,
  otlp_logs_endpoint: validUrlSchema,
});

// --------------------------------------------------
// OPEN TELEMETRY
// --------------------------------------------------
export const openTelemetrySchema = z
  .object({
    /**
     * If OpenTelemetry is enabled or not. Allows to enable/disable the feature without losing the
     * configuration and/or inferring the status from other data (initially, data_types meant that
     * OpenTelemetry is disabled if it is empty but this is not true anymore because of the bad UX
     * consequences).
     */
    status: z.enum(['enabled', 'disabled']),

    /**
     * The individually-enabled telemetry export types
     */
    data_types: z.array(z.enum(['traces', 'metrics', 'logs'])),

    batch_span_processor: z.object({
      // a value between 1 and 512
      max_export_batch_size: z.number().min(1).max(512),
    }),

    exporter_otlp: exporterSchema.extend({
      // NOTE: refine validation below assumes a non-falsy value is valid URL
      otlp_traces_endpoint: validUrlSchema
        .or(z.literal(''))
        .or(z.literal(undefined)),
      otlp_metrics_endpoint: validUrlSchema
        .or(z.literal(''))
        .or(z.literal(undefined)),
      otlp_logs_endpoint: validUrlSchema
        .or(z.literal(''))
        .or(z.literal(undefined)),
    }),
  })
  // enforce invariant that: when export is enabled globally AND when the
  // corresponding data_type is enabled THEN a valid endpoint url is provided.
  .refine(
    obj =>
      obj.status === 'enabled' && obj.data_types.includes('traces')
        ? obj.exporter_otlp.otlp_traces_endpoint
        : true,
    {
      message:
        'A valid traces endpoint must be supplied when trace export is enabled',
      path: ['exporter_otlp', 'otlp_traces_endpoint'],
    }
  )
  .refine(
    obj =>
      obj.status === 'enabled' && obj.data_types.includes('metrics')
        ? obj.exporter_otlp.otlp_metrics_endpoint
        : true,
    {
      message:
        'A valid metrics endpoint must be supplied when metrics export is enabled',
      path: ['exporter_otlp', 'otlp_metrics_endpoint'],
    }
  )
  .refine(
    obj =>
      obj.status === 'enabled' && obj.data_types.includes('logs')
        ? obj.exporter_otlp.otlp_logs_endpoint
        : true,
    {
      message:
        'A valid logs endpoint must be supplied when logs export is enabled',
      path: ['exporter_otlp', 'otlp_logs_endpoint'],
    }
  );

export type OpenTelemetry = z.infer<typeof openTelemetrySchema>;
