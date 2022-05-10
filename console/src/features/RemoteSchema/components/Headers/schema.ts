import { z } from 'zod';

export const headersSchema = z.array(
  z.object({
    name: z.string(),
    value: z.string(),
    type: z.literal('from_env').or(z.literal('from_value')),
  })
);

export type THeaders = z.infer<typeof headersSchema>;
