import { z } from 'zod';
import { requestHeadersSelectorSchema } from '../../../../../new-components/RequestHeadersSelector';

const endPointSchema = z.string().url({ message: 'Invalid URL' });

// --------------------------------------------------
// SCHEMA
// --------------------------------------------------
const normalProperties = {
  // CONNECTION TYPE
  connectionType: z.enum(['http/protobuf']),

  dataType: z.enum(['traces']).array(),

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
};

export const formSchema = z.discriminatedUnion('enabled', [
  z.object({
    ...normalProperties,

    enabled: z.literal(true),
    endpoint: endPointSchema,
  }),
  z.object({
    ...normalProperties,

    enabled: z.literal(false),
    endpoint: z.union([
      endPointSchema,
      // When OpenTelemetry is disabled, the endpoint is not mandatory
      z.literal(''),
    ]),
  }),
]);

// --------------------------------------------------
// FORM VALUES
// --------------------------------------------------
export type FormValues = z.infer<typeof formSchema>;

export const defaultValues: FormValues = {
  enabled: false,

  // At the time of writing, it's impossible to get a default value that satisfies the use cases.
  // localhost would not work because HGE is running inside Docker, and the OpenTelemetry host is not.
  endpoint: '',

  // At the time of writing, the server sets 512 as the default value.
  batchSize: 512,

  connectionType: 'http/protobuf',

  dataType: ['traces'],

  headers: [],
  attributes: [],
};
