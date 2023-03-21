import pickBy from 'lodash/pickBy';
import { z } from 'zod';

export const schema = z
  .object({
    comment: z.string().optional(),
    custom_name: z.string().optional(),
  })
  // we need to do this transform bc the server rejects a property with an empty value
  // so, the only way to remove a comment/custom_name is to send a new object WITHOUT the property
  .transform(value => pickBy(value, d => d !== ''));

export type Schema = z.infer<typeof schema>;
