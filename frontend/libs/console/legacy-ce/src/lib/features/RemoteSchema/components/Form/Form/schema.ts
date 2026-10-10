import { z } from 'zod';
import { requestHeadersSelectorSchema } from '@hasura/shared/ui';

export const schema = z.object({
  name: z.string().min(1, 'Remote Schema name is a required field!'),
  url: z.object({
    value: z.string(),
    type: z.literal('from_url').or(z.literal('from_env')),
  }),
  headers: requestHeadersSelectorSchema,
  // Tracks whether the user has explicitly opted in to a separate list of
  // introspection headers. When false, `introspection_headers` is omitted from
  // the metadata so introspection inherits the ordinary request `headers`.
  use_introspection_headers: z.boolean(),
  introspection_headers: requestHeadersSelectorSchema,
  forward_client_headers: z.boolean(),
  // A cleared number input is NaN; `transformFormData` turns it into the
  // default timeout.
  timeout_seconds: z.union([z.number().min(1), z.nan()]).default(60),
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
        (data) => {
          if ((data.prefix || data.suffix) && !data.parent_type) return false;
          return true;
        },
        {
          message: 'Query type name cannot be empty!',
        },
      ),
    mutation_root: z
      .object({
        parent_type: z.string(),
        prefix: z.string(),
        suffix: z.string(),
      })
      .refine(
        (data) => {
          if ((data.prefix || data.suffix) && !data.parent_type) return false;
          return true;
        },
        {
          message: 'Mutation type name cannot be empty!',
        },
      ),
  }),
  comment: z.string().optional(),
});

export type Schema = z.infer<typeof schema>;
