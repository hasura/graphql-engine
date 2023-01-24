import { z } from 'zod';

export const schema = z.object({
  name: z.string().min(1, 'Name is a required field'),
  relationship_type: z.union([z.literal('Object'), z.literal('Array')]),
  fromSource: z.object({
    value: z.object({
      dataSourceName: z.string().min(1, 'From source is a required field'),
      table: z.any(),
    }),
  }),
  toSource: z.object({
    value: z.object({
      dataSourceName: z
        .string()
        .min(1, 'To Reference source is a required field'),
      table: z.any(),
    }),
  }),
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
});

export type Schema = z.infer<typeof schema>;
