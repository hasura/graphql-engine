import { z } from 'zod';

export const requestHeadersSelectorSchema = z.array(
  z.object({
    name: z.string(),
    value: z.string(),
    type: z.literal('from_env').or(z.literal('from_value')),
  })
);

export type RequestHeadersSelectorSchema = z.infer<
  typeof requestHeadersSelectorSchema
>;
