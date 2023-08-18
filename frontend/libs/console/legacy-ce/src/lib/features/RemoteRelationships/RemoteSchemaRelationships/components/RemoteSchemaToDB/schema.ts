import { z } from 'zod';

export const schema = z.object({
  relationshipType: z.literal('array').or(z.literal('object')),
  relationshipName: z.string().min(1, { message: 'Name is required!' }),
  target: z.object({
    type: z.literal('table'),
    dataSourceName: z.string().min(1, 'Reference source is a required field'),
    table: z.any(),
  }),
  mapping: z.array(
    z.object({
      field: z.string(),
      column: z.string(),
    })
  ),
  typeName: z.string().min(1, { message: 'Type is required!' }),
  sourceRemoteSchema: z.string(),
  driver: z.string(),
});

export type Schema = z.infer<typeof schema>;
