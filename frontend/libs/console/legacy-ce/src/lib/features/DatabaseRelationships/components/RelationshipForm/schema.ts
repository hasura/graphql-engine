import { z } from 'zod';

const dbTodbSchema = z.object({
  name: z.string().min(1, 'Name is a required field'),
  fromSource: z.object({
    type: z.literal('table'),
    dataSourceName: z.string().min(1, 'Origin source is a required field'),
    table: z.any(),
  }),
  toSource: z.object({
    type: z.literal('table'),
    dataSourceName: z.string().min(1, 'Reference source is a required field'),
    table: z.any(),
  }),
  details: z.object({
    relationshipType: z.union([z.literal('Object'), z.literal('Array')]),
    columnMap: z
      .array(z.object({ from: z.string(), to: z.string() }))
      .transform((columnMap, ctx) => {
        return columnMap.map(map => {
          if (!map.to || !map.from)
            ctx.addIssue({
              code: z.ZodIssueCode.custom,
              message: `Column Mapping cannot be empty`,
            });

          return map;
        });
      }),
  }),
});

export const dbToRsSchema = z.object({
  name: z.string().min(1, 'Name is a required field'),
  fromSource: z.object({
    type: z.literal('table'),
    dataSourceName: z.string().min(1, 'Origin source is a required field'),
    table: z.any(),
  }),
  toSource: z.object({
    type: z.literal('remoteSchema'),
    remoteSchema: z.string().min(1, 'Remote Schema is a required field'),
  }),
  details: z.object({
    rsFieldMapping: z.any(),
  }),
});

export const schema = z.union([dbTodbSchema, dbToRsSchema]);

export type Schema = z.infer<typeof schema>;
