import { z } from 'zod';
import type { SchemaTable } from '@hasura/shared/types';
import type {
  ForeignKeyFormSchema,
  ModifyTableArgs,
} from '@hasura/metadata/data-source';

export const columnSchema = z.object({
  name: z.string(),
  type: z.string(),
  nullable: z.boolean(),
  array: z.boolean(),
  unique: z.boolean(),
  default: z.string().optional(),
  isPrimaryKey: z.boolean(),
});

export const checkConstraintSchema = z.object({
  name: z.string(),
  check: z.string(),
});

// A composite unique key captured by the form: a set of column names.
export const uniqueKeySchema = z.object({
  columns: z.array(z.string()),
});

// A single foreign key definition captured by the form. `referenceTable` is
// stored as a JSON-encoded `Table` (the select value); the mapping below parses
// it into the `ForeignKeyFormSchema` shape consumed by `createTable`.
export const foreignKeySchema = z.object({
  referenceTable: z.unknown(),
  columnMappings: z.array(
    z.object({
      from: z.string(),
      to: z.string(),
    }),
  ),
  onUpdate: z.string().optional(),
  onDelete: z.string().optional(),
});

export const addTableSchema = z
  .object({
    name: z.string().min(1, { message: 'Table name is required' }),
    schema: z.string().min(1, { message: 'Schema is required' }),
    comment: z.string().optional(),
    columns: z.array(columnSchema),
    checkConstraints: z.array(checkConstraintSchema),
    foreignKeys: z.array(foreignKeySchema),
    uniqueKeys: z.array(uniqueKeySchema),
  })
  .superRefine((values, ctx) => {
    const namedColumns = values.columns.filter((c) => c.name.trim() !== '');

    if (namedColumns.length === 0) {
      ctx.addIssue({
        code: z.ZodIssueCode.custom,
        path: ['columns'],
        message: 'At least one column is required',
      });
    }

    const names = namedColumns.map((c) => c.name.trim());
    if (new Set(names).size !== names.length) {
      ctx.addIssue({
        code: 'custom',
        path: ['columns'],
        message: 'Column names must be unique',
      });
    }

    namedColumns.forEach((c) => {
      if (!c.type) {
        ctx.addIssue({
          code: 'custom',
          path: ['columns'],
          message: `Column "${c.name}" requires a data type`,
        });
      }
    });

    if (!namedColumns.some((c) => c.isPrimaryKey)) {
      ctx.addIssue({
        code: z.ZodIssueCode.custom,
        path: ['columns'],
        message: 'At least one primary key column is required',
      });
    }

    // Reject incomplete foreign keys the user actually started editing, rather
    // than silently dropping them in the payload mapping.
    values.foreignKeys.forEach((fk, i) => {
      const touched =
        fk.referenceTable !== '' ||
        fk.columnMappings.some((m) => m.from !== '' || m.to !== '');
      if (!touched) return;

      if (fk.referenceTable === '') {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          path: ['foreignKeys', i, 'referenceTable'],
          message: 'Select a reference table',
        });
      }

      const completeMappings = fk.columnMappings.filter(
        (m) => m.from !== '' && m.to !== '',
      );
      if (completeMappings.length === 0) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          path: ['foreignKeys', i, 'columnMappings'],
          message: 'Add at least one complete column mapping',
        });
      }

      fk.columnMappings.forEach((m, j) => {
        if ((m.from === '') !== (m.to === '')) {
          ctx.addIssue({
            code: z.ZodIssueCode.custom,
            path: ['foreignKeys', i, 'columnMappings', j],
            message: 'Both the source and reference columns are required',
          });
        }
      });
    });

    // A composite unique key row must reference at least one column.
    values.uniqueKeys.forEach((uk, i) => {
      if (uk.columns.filter((c) => c !== '').length === 0) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          path: ['uniqueKeys', i, 'columns'],
          message: 'Select at least one column for the unique key',
        });
      }
    });
  });

export type AddTableFormValues = z.infer<typeof addTableSchema>;

export const defaultColumn: z.infer<typeof columnSchema> = {
  name: '',
  type: '',
  nullable: false,
  array: false,
  unique: false,
  default: '',
  isPrimaryKey: false,
};

export const addTableDefaultValues: AddTableFormValues = {
  name: '',
  schema: '',
  comment: '',
  columns: [{ ...defaultColumn }],
  checkConstraints: [],
  foreignKeys: [],
  uniqueKeys: [],
};

/**
 * Convert a driver `FrequentlyUsedColumn` preset (from
 * `getFrequentlyUsedColumns()`) into an Add Table column row.
 */
export function frequentlyUsedColumnToFormColumn(preset: {
  name: string;
  type: string;
  default?: string;
  primary?: boolean;
}): z.infer<typeof columnSchema> {
  return {
    ...defaultColumn,
    name: preset.name,
    type: preset.type,
    default: preset.default ?? '',
    isPrimaryKey: Boolean(preset.primary),
  };
}

// Append a `[]` array suffix without double-suffixing a type that already is an
// array (defensive against type maps that already expose array types).
const toColumnType = (type: string, array: boolean): string =>
  array && !type.endsWith('[]') ? `${type}[]` : type;

/**
 * Maps the Add Table form values into the driver-agnostic `ModifyTableArgs`
 * payload consumed by `useCreateTable`. Only non-empty (named) columns are
 * emitted; `primaryKeys`/`uniqueKeys` are indices into that filtered list, so
 * they line up with the SQL builder which also filters empty columns.
 */
export function formValuesToCreateTableArgs(
  values: AddTableFormValues,
): ModifyTableArgs {
  const namedColumns = values.columns.filter((c) => c.name.trim() !== '');

  const table: SchemaTable = {
    name: values.name.trim(),
    schema: values.schema,
  };

  return {
    table,
    columns: namedColumns.map((c) => ({
      name: c.name.trim(),
      type: toColumnType(c.type, c.array),
      nullable: c.nullable,
      ...(c.default && c.default.trim() !== ''
        ? { default: { value: c.default } }
        : {}),
    })),
    primaryKeys: namedColumns
      .map((c, index) => (c.isPrimaryKey ? index : -1))
      .filter((index) => index >= 0),
    foreignKeys: values.foreignKeys
      .map((fk): ForeignKeyFormSchema | null => {
        const referenceTable = fk.referenceTable;
        const columnMappings = fk.columnMappings.filter(
          (m) => m.from !== '' && m.to !== '',
        );
        if (!referenceTable || columnMappings.length === 0) return null;
        return {
          referenceTable,
          columnMappings,
          onUpdate:
            (fk.onUpdate as ForeignKeyFormSchema['onUpdate']) ?? undefined,
          onDelete:
            (fk.onDelete as ForeignKeyFormSchema['onDelete']) ?? undefined,
        };
      })
      .filter((fk): fk is ForeignKeyFormSchema => fk !== null),
    // Unique keys come from two sources, merged and de-duplicated:
    //  - per-column `unique` flags -> single-column keys
    //  - composite `uniqueKeys` rows -> multi-column keys
    // Indices are relative to the filtered (named) columns to match the SQL
    // builder. De-duplication uses the sorted-index signature so a per-column
    // unique and an equivalent composite key don't emit duplicate constraints.
    uniqueKeys: (() => {
      const columnIndex = (name: string) =>
        namedColumns.findIndex((c) => c.name.trim() === name);

      const perColumn = namedColumns
        .map((c, index) => (c.unique && !c.isPrimaryKey ? [index] : null))
        .filter((entry): entry is number[] => entry !== null);

      const composite = values.uniqueKeys
        .map((uk) =>
          uk.columns
            .map((name) => columnIndex(name))
            .filter((index) => index >= 0),
        )
        .filter((indices) => indices.length > 0);

      const seen = new Set<string>();
      const result: number[][] = [];
      for (const key of [...perColumn, ...composite]) {
        const signature = [...key].sort((a, b) => a - b).join(',');
        if (seen.has(signature)) continue;
        seen.add(signature);
        result.push(key);
      }
      return result;
    })(),
    checkConstraints: values.checkConstraints.filter(
      (c) => c.name.trim() !== '' && c.check.trim() !== '',
    ),
    ...(values.comment && values.comment.trim() !== ''
      ? { tableComment: values.comment.trim() }
      : {}),
  };
}
