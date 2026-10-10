import { z } from 'zod';
import { reqString } from '@hasura/shared/utils';

export const schema = z.object({
  root_field_name: reqString('Native Query Name'),
  comment: z.string().optional(),
  source: reqString('Database'),
  type: z.enum(['query', 'mutation']).default('query').optional(),
  arguments: z
    .object({
      name: reqString('Parameter Name'),
      type: reqString('Parameter Type'),
      description: z.string().optional(),
      nullable: z.boolean().optional(),
    })
    .array(),
  code: reqString('Sql Query'),
  returns: reqString('Query Return Type'),
  array_relationships: z
    .array(
      z.object({
        name: z.string(),
        using: z.object({
          column_mapping: z.record(z.string(), z.string()),
          insertion_order: z.union([
            z.literal('before_parent'),
            z.literal('after_parent'),
            z.literal(null),
          ]),
          remote_native_query: z.string(),
        }),
      }),
    )
    .optional(),
  object_relationships: z
    .array(
      z.object({
        name: z.string(),
        using: z.object({
          column_mapping: z.record(z.string(), z.string()),
          insertion_order: z.union([
            z.literal('before_parent'),
            z.literal('after_parent'),
            z.literal(null),
          ]),
          remote_native_query: z.string(),
        }),
      }),
    )
    .optional(),
});
