import { z } from 'zod';

export const schema = z.object({
  name: z.string().min(1, 'Name is a required field!'),
  relationship_type: z.union([z.literal('object'), z.literal('array')], {
    required_error: 'Relationship type is required field!',
  }),
  source_table: z.any().transform((value) => {
    try {
      return JSON.parse(value);
    } catch {
      return value;
    }
  }),
  source_name: z.string().min(1, 'Source source must be provided!'),
  target_name: z.string().min(1, 'Reference source must be provided!'),
  target_table: z.any().transform((value) => {
    try {
      return JSON.parse(value);
    } catch {
      return null;
    }
  }),
  column_mapping: z
    .array(
      z.object({
        from: z.string().min(1, 'Please provide a column!'),
        to: z.string().min(1, 'Please provide a column!'),
      })
    )
    .min(1, 'Please provide at least one column mapping!'),
});

export type Schema = z.infer<typeof schema>;
