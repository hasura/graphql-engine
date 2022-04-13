import { z } from 'zod';

export const schema = z.object({
  relationshipType: z.literal('array').or(z.literal('object')),
  relationshipName: z.string().min(1, { message: 'Name is required!' }),
  database: z.string().min(1, { message: 'Database is required!' }),
  schema: z.string().min(1, { message: 'Schema is required!' }),
  table: z.string().min(1, { message: 'Table is required!' }),
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
