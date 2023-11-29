import { z } from 'zod';
import { requestHeadersSelectorSchema } from '../../../../../new-components/RequestHeadersSelector';

const endPointSchema = z.string();

// --------------------------------------------------
// SCHEMA
// --------------------------------------------------

export const tracesPropagatorSchema = z.enum(['b3', 'tracecontext']);

export const formSchema = z
  .object({
    // CONNECTION TYPE
    connectionType: z.enum(['http/protobuf']),
    enabled: z.boolean(),

    dataType: z.enum(['traces', 'metrics', 'logs']).array(),
    // NOTE: We enforce more specific invariants below:
    tracesEndpoint: z.union([endPointSchema, z.literal('')]),
    metricsEndpoint: z.union([endPointSchema, z.literal('')]),
    logsEndpoint: z.union([endPointSchema, z.literal('')]),

    // HEADERS
    // Names should be validated against /^[a-zA-Z0-9]*$/ but we must be sure the server performs the
    // same check. Values should be validated against /^[a-zA-Z0-9_ :;.,\\\/"'\?\!\(\)\{\}\[\]@<>=\-+\*#$&`|~^%]*$/
    // see: the CloudFlare docs as an example https://developers.cloudflare.com/rules/transform/request-header-modification/reference/header-format/#:~:text=The%20value%20of%20the%20HTTP,%24%26%60%7C~%5E%25
    // More: empty env vars should not be accepted!
    headers: requestHeadersSelectorSchema,

    // ATM, only string values are accepted out of the wide variety of values OpenTelemetry accepts
    // see: https://opentelemetry.io/docs/reference/specification/common/#attribute
    // ATTENTION: a restricted version of requestHeadersSelectorSchema should be used here! Because
    // the attributes cannot be sent as env vars, even if the same RequestHeadersSelector component is
    // used. RequestHeadersSelector accepts a typeSelect prop but the schema does not reflect it!
    attributes: requestHeadersSelectorSchema,

    batchSize: z.coerce
      .number()
      // The message is the same for min and max to avoid a "The value should be greater than 1"
      // error and then another "The value should be lower than 512"  one. By using the same message,
      // the user will only see one error and understand everything at once.
      .min(1, { message: 'The value should be between 1 and 512' })
      .max(512, { message: 'The value should be between 1 and 512' }),

    // Enable extra trace propagators besides b3
    tracesPropagators: tracesPropagatorSchema.array(),
  })
  // enforce invariant that: when export is enabled globally AND when the
  // corresponding data_type is enabled THEN a valid endpoint url is provided.
  .refine(
    obj =>
      obj.enabled && obj.dataType.includes('traces')
        ? obj.tracesEndpoint
        : true,
    {
      message:
        'A valid traces endpoint must be supplied when trace export is enabled',
      path: ['tracesEndpoint'],
    }
  )
  .refine(
    obj =>
      obj.enabled && obj.dataType.includes('metrics')
        ? obj.metricsEndpoint
        : true,
    {
      message:
        'A valid metrics endpoint must be supplied when metrics export is enabled',
      path: ['metricsEndpoint'],
    }
  )
  .refine(
    obj =>
      obj.enabled && obj.dataType.includes('logs') ? obj.logsEndpoint : true,
    {
      message:
        'A valid logs endpoint must be supplied when logs export is enabled',
      path: ['logsEndpoint'],
    }
  );

// --------------------------------------------------
// FORM VALUES
// --------------------------------------------------
export type FormValues = z.infer<typeof formSchema>;

export const defaultValues: FormValues = {
  enabled: false,

  // At the time of writing, it's impossible to get a default value that satisfies the use cases.
  // localhost would not work because HGE is running inside Docker, and the OpenTelemetry host is not.
  tracesEndpoint: '',
  metricsEndpoint: '',
  logsEndpoint: '',

  // At the time of writing, the server sets 512 as the default value.
  batchSize: 512,

  connectionType: 'http/protobuf',

  dataType: ['traces', 'metrics', 'logs'],

  headers: [],
  attributes: [],
  tracesPropagators: [],
};
