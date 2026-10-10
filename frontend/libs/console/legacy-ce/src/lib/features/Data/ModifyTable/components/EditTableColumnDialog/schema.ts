import pickBy from 'lodash/pickBy';
import { z } from 'zod';

export const schema = z
  .object({
    comment: z.string().optional(),
    custom_name: z.string().optional(),
  })
  // we need to do this transform bc the server rejects a property with an empty value
  // so, the only way to remove a comment/custom_name is to send a new object WITHOUT the property
  .transform((value) => pickBy(value, (d) => d !== ''));

export type Schema = z.infer<typeof schema>;

/** The full edit form: database column definition + metadata column config.
 *  The database fields are only editable when the driver can alter columns. */
export const columnFormSchema = z.object({
  name: z.string().trim().min(1, { message: 'Column name is required' }),
  type: z.string().trim().min(1, { message: 'Column type is required' }),
  nullable: z.boolean(),
  unique: z.boolean(),
  default: z.string(),
  comment: z.string(),
  custom_name: z.string(),
});

export type ColumnFormValues = z.infer<typeof columnFormSchema>;
