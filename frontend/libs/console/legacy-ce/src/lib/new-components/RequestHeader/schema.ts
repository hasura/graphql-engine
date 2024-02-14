import { z } from 'zod';

export const requestHeadersSchema = z.array(
  z.object({
    name: z.string(),
    value: z.string(),
  })
);

export type RequestHeadersSchema = z.infer<typeof requestHeadersSchema>;
