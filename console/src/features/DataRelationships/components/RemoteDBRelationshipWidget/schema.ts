import { z } from 'zod';

export const schema = z.object({
  relationshipType: z.literal('array').or(z.literal('object')),
  relationshipName: z.string().min(1, { message: 'Name is required!' }),
  source: z.object({
    database: z.string().min(1, 'Source Database is required!'),
    schema: z.string().optional(),
    dataset: z.string().optional(),
    table: z.string().min(1, 'Source Table is required!'),
  }),
  destination: z.object({
    database: z.string().min(1, 'Reference Database is required!'),
    schema: z.string().optional(),
    dataset: z.string().optional(),
    table: z.string().min(1, 'Reference Table is required!'),
  }),
  mapping: z.record(z.string()),
});

export type Schema = z.infer<typeof schema>;
