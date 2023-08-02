import { z } from 'zod';

export const schema = z.object({
  name: z.string().min(1, 'Name is a required field'),
  fromSource: z.object({
    dataSourceName: z.string().min(1, 'Origin source is a required field'),
    table: z.any(),
  }),
  toRemoteSchema: z.string().min(1, 'Remote Schema is a required field'),
  remoteSchemaFieldMapping: z.any(),
});

export type Schema = z.infer<typeof schema>;
