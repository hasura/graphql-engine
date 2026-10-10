import { z } from 'zod';
import { requestHeadersSelectorSchema } from '@hasura/shared/ui';
import { isJsonString } from '@hasura/shared/utils';

// The retry fields are `type: 'number'` inputs, which store a number (NaN
// when empty); the rest of this form works with their string form.
const numberInputAsString = z.preprocess(
  (value) =>
    typeof value === 'number'
      ? Number.isNaN(value)
        ? ''
        : String(value)
      : value,
  z.string(),
);

export const schema = z.object({
  name: z.string().min(1, 'Cron Trigger name is a required field!'),
  webhook: z.string().min(1, 'Webhook url is a required field!'),
  schedule: z.string().min(1, 'Cron Schedule is a required field!'),
  payload: z.string().refine((arg: string) => isJsonString(arg), {
    message: 'Payload must be valid json',
  }),
  headers: requestHeadersSelectorSchema,
  num_retries: numberInputAsString,
  retry_interval_seconds: numberInputAsString,
  timeout_seconds: numberInputAsString,
  tolerance_seconds: numberInputAsString,
  include_in_metadata: z.boolean(),
  comment: z.string(),
});

export type Schema = z.infer<typeof schema>;
