import { z } from 'zod';

export const schema = z.object({
  relationshipName: z.string().min(1, { message: 'Name is required!' }),
  referenceRemoteSchema: z
    .string()
    .min(1, { message: 'Related remote schema is required' }),
  resultSet: z.any(),
  relationships: z.any(),
});

export type DbToRsSchema = z.infer<typeof schema>;
