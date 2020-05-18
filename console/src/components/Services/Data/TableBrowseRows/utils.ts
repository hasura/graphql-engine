export type Query = {
  name?: string;
  columns: (Query | string)[];
};

const isQuery = (x: unknown): x is Query => {
  return typeof x === 'object' && (x as Query).columns !== undefined;
};

export const getSelectedColumns = (
  query: Query,
  currentTable: string,
  tableName: string
): string[] => {
  if (currentTable === tableName || query.name === tableName)
    return query.columns.filter(col => typeof col === 'string') as string[];

  const nestedQuery = query.columns.find(isQuery) as Query;
  if (nestedQuery) {
    return getSelectedColumns(nestedQuery, currentTable, tableName);
  }
  return [];
};

export const setSelectedColumns = (
  query: Query,
  currentTable: string,
  tableName: string,
  columns: string[]
): Query => {
  const nestedQuery = query.columns.find(isQuery) as Query;
  if (currentTable === tableName || query.name === tableName) {
    const newColumns = columns as (Query | string)[];
    if (nestedQuery) {
      newColumns.push(nestedQuery);
    }
    return { ...query, columns: newColumns };
  }
  if (nestedQuery) {
    const currTableCols = query.columns.filter(col => typeof col === 'string');
    const newQuery = {
      ...query,
      columns: [
        ...currTableCols,
        setSelectedColumns(nestedQuery, currentTable, tableName, columns),
      ],
    };
    return newQuery;
  }
  return query;
};

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
