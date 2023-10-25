import { z } from 'zod';

export const rsToRsFormSchema = z.object({
  relationshipMethod: z.literal('remoteSchema').or(z.literal('remoteDatabase')),
  name: z.string().min(1, { message: 'Name is required' }),
  sourceRemoteSchema: z.string(),
  rsSourceType: z.string().min(1, 'Source type is required'),
  referenceRemoteSchema: z
    .string()
    .min(1, { message: 'Related remote schema is required' }),
  selectedOperation: z.string(),
  resultSet: z.any(),
  relationship: z.any(),
});

export type RsToRsSchema = z.infer<typeof rsToRsFormSchema>;
