import { z } from 'zod';
import { headersSchema } from '../Headers';

export const schema = z.object({
  name: z.string().min(1, 'Remote Schema name is a required field!'),
  url: z.string().url().min(1, 'URL is a required field!'),
  headers: headersSchema,
  forward_client_headers: z.preprocess(val => {
    if (val === 'true') return true;
    return false;
  }, z.boolean()),
  timeout_seconds: z.string().optional(),
  customization: z.object({
    root_fields_namespace: z.string(),
    type_prefix: z.string(),
    type_suffix: z.string(),
    query_root: z
      .object({
        parent_type: z.string(),
        prefix: z.string(),
        suffix: z.string(),
      })
      .refine(
        data => {
          if ((data.prefix || data.suffix) && !data.parent_type) return false;
          return true;
        },
        {
          message: 'Query type name cannot be empty!',
        }
      ),
    mutation_root: z
      .object({
        parent_type: z.string(),
        prefix: z.string(),
        suffix: z.string(),
      })
      .refine(
        data => {
          if ((data.prefix || data.suffix) && !data.parent_type) return false;
          return true;
        },
        {
          message: 'Mutation type name cannot be empty!',
        }
      ),
  }),
  comment: z.string().optional(),
});

export type Schema = z.infer<typeof schema>;
