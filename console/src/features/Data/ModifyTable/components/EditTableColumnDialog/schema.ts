import pickBy from 'lodash.pickby';
import { z } from 'zod';

export const schema = z
  .object({
    comment: z.string().optional(),
    custom_name: z.string().optional(),
  })
  .transform(value => pickBy(value, d => d !== ''));

export type Schema = z.infer<typeof schema>;
