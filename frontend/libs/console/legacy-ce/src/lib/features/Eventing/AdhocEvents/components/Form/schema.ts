import { z } from 'zod';
import { requestHeadersSelectorSchema } from '@hasura/shared/ui';
import { isJsonString } from '@hasura/shared/utils';

export const schema = z.object({
  webhook: z.string().min(1, 'Webhook url is a required field!'),
  time: z.union([z.string(), z.any()]),
  payload: z.string().refine((arg: string) => isJsonString(arg), {
    message: 'Payload must be valid json',
  }),
  headers: requestHeadersSelectorSchema,
  num_retries: z.number().min(0),
  retry_interval_seconds: z.number().min(1),
  timeout_seconds: z.number().min(1),
  comment: z.string(),
});

export type Schema = z.infer<typeof schema>;
