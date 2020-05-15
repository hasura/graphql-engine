type TableSchema = {
  primary_key?: { columns: string[] };
  columns: Array<{ column_name: string }>;
};

type TableSchemaWithPK = {
  [P in keyof TableSchema]-?: TableSchema[P];
};

export const isTableWithPK = (
  tableSchema: TableSchema
): tableSchema is TableSchemaWithPK => {
  return (
    !!tableSchema.primary_key && tableSchema.primary_key.columns.length > 0
  );
};

export const compareRows = (
  row1: Record<string, any>,
  row2: Record<string, any>,
  tableSchema: TableSchema,
  isView: boolean
) => {
  let same = true;
  if (!isView && isTableWithPK(tableSchema)) {
    tableSchema.primary_key.columns.forEach(pk => {
      if (row1[pk] !== row2[pk]) {
        same = false;
      }
    });
    return same;
  }
  tableSchema.columns.forEach(k => {
    if (row1[k.column_name] !== row2[k.column_name]) {
      same = false;
    }
  });
  return same;
};
