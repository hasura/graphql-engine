import { z } from 'zod';
import { requestHeadersSelectorSchema } from '@/new-components/RequestHeadersSelector';

export const formSchema = z.object({
  enabled: z.boolean(),

  endpoint: z.string().url({ message: 'Invalid URL' }),

  connectionType: z.enum(['http', 'http2']),

  dataType: z.enum(['traces']).array(),

  headers: requestHeadersSelectorSchema,

  // ATTENTION: a restricted version of requestHeadersSelectorSchema should be used here! Because
  // the attributes cannot be sent as env vars, even if the same RequestHeadersSelector component is
  // used. RequestHeadersSelector accepts a typeSelect prop but the schema does not reflect it!
  attributes: requestHeadersSelectorSchema,

  // TODO: migrate to coerce
  batchSize: z.preprocess(
    // see: https://github.com/colinhacks/zod/discussions/330#discussioncomment-4043200
    Number,
    z
      .number()
      // The message is the same for min and max to avoid a "The value should be greater than 1"
      // error and then another "The value should be lower than 512"  one. By using the same message,
      // the user will only see one error and understand everything at once.
      .min(1, { message: 'The value should be between 1 and 512' })
      .max(512, { message: 'The value should be between 1 and 512' })
  ),
});

export type FormValues = z.infer<typeof formSchema>;

export const defaultValues: FormValues = {
  enabled: false,

  // At the time of writing, it's impossible to get a default value that satisfies the use cases.
  // localhost would not work because HGE is running inside Docker, and the OpenTelemetry host is not.
  endpoint: '',

  // At the time of writing, the server sets 512 as the default value.
  batchSize: 512,

  connectionType: 'http',

  dataType: ['traces'],

  // An empty element allows having the first line to be there when the collapsible opens.
  // TODO: the behavior should be encapsulated in the RequestHeadersSelector because it's hard to
  // maintain this UX when the server sends down the config without any headers...
  headers: [{ name: '', type: 'from_value', value: '' }],
  attributes: [{ name: '', type: 'from_value', value: '' }],
};
