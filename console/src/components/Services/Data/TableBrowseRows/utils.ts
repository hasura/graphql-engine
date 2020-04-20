export type Query = {
  name: string;
  columns: (Query | string)[];
};

const isQuery = (x: unknown): x is Query => {
  return (
    typeof x === 'object' &&
    (x as Query).columns !== undefined &&
    (x as Query).name !== undefined
  );
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
