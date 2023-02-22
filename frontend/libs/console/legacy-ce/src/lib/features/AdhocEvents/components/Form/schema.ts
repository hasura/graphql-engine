import { z } from 'zod';
import { requestHeadersSelectorSchema } from '../../../../new-components/RequestHeadersSelector';
import { isJsonString } from '../../../../components/Common/utils/jsUtils';

export const schema = z.object({
  webhook: z.string().min(1, 'Webhook url is a required field!'),
  time: z.union([z.string(), z.any()]),
  payload: z.string().refine((arg: string) => isJsonString(arg), {
    message: 'Payload must be valid json',
  }),
  headers: requestHeadersSelectorSchema,
  num_retries: z.string(),
  retry_interval_seconds: z.string(),
  timeout_seconds: z.string(),
  comment: z.string(),
});

export type Schema = z.infer<typeof schema>;
