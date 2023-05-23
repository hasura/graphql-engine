import { z } from 'zod';
import { requestHeadersSelectorSchema } from '../../../../new-components/RequestHeadersSelector';
import { isJsonString } from '../../../../components/Common/utils/jsUtils';

export const schema = z.object({
  name: z.string().min(1, 'Cron Trigger name is a required field!'),
  webhook: z.string().min(1, 'Webhook url is a required field!'),
  schedule: z.string().min(1, 'Cron Schedule is a required field!'),
  payload: z.string().refine((arg: string) => isJsonString(arg), {
    message: 'Payload must be valid json',
  }),
  headers: requestHeadersSelectorSchema,
  num_retries: z.string(),
  retry_interval_seconds: z.string(),
  timeout_seconds: z.string(),
  tolerance_seconds: z.string(),
  include_in_metadata: z.boolean(),
  comment: z.string(),
});

export type Schema = z.infer<typeof schema>;
