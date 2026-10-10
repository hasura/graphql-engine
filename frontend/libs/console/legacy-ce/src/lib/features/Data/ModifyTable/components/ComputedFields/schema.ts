import { z } from 'zod';

export const schema = z.object({
  name: z.string().min(1, 'Computed field name is required'),
  function: z.unknown().refine((val) => Boolean(val), {
    message: 'Function is required',
  }),
  table_argument: z.string().optional(),
  session_argument: z.string().optional(),
  comment: z.string().optional(),
});

export type Schema = z.infer<typeof schema>;
