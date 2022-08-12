import * as z from 'zod';

export const schema = z.object({
  checkType: z.string(),
  filterType: z.string(),
  check: z.string(),
  filter: z.string(),
  rowCount: z.string(),
  columns: z.record(z.optional(z.boolean())),
  presets: z.optional(
    z.array(
      z.object({
        columnName: z.string(),
        presetType: z.optional(z.string()),
        columnValue: z.optional(z.union([z.string(), z.number()])),
      })
    )
  ),
  aggregationEnabled: z.boolean(),
  backendOnly: z.boolean(),
  clonePermissions: z.optional(
    z.array(
      z.object({
        tableName: z.optional(z.string()),
        queryType: z.optional(z.string()),
        roleName: z.optional(z.string()),
      })
    )
  ),
});
