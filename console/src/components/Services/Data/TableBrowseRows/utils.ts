import { Operators } from '../constants';

export type Query = {
  name?: string;
  columns: (Query | string)[];
};

// const isQuery = (x: unknown): x is Query => {
//   return typeof x === 'object' && (x as Query).columns !== undefined;
// };

// export const getSelectedColumns = (
//   query: Query,
//   currentTable: string,
//   tableName: string // root table name
// ): string[] => {
//   if (currentTable === tableName || query.name === tableName)
//     return query.columns.filter(col => typeof col === 'string') as string[];

//   const nestedQuery = query.columns.find(isQuery) as Query;
//   if (nestedQuery) {
//     return getSelectedColumns(nestedQuery, currentTable, tableName);
//   }
//   return [];
// };

// export const setSelectedColumns = (
//   query: Query,
//   currentTable: string,
//   tableName: string,
//   columns: string[]
// ): Query => {
//   const nestedQuery = query.columns.find(isQuery) as Query;
//   if (currentTable === tableName || query.name === tableName) {
//     const newColumns = columns as (Query | string)[];
//     if (nestedQuery) {
//       newColumns.push(nestedQuery);
//     }
//     return { ...query, columns: newColumns };
//   }
//   if (nestedQuery) {
//     const currTableCols = query.columns.filter(col => typeof col === 'string');
//     const newQuery = {
//       ...query,
//       columns: [
//         ...currTableCols,
//         setSelectedColumns(nestedQuery, currentTable, tableName, columns),
//       ],
//     };
//     return newQuery;
//   }
//   return query;
// };

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

export const getDefaultValue = (possibleValue: unknown, opName: string) => {
  if (possibleValue) {
    if (Array.isArray(possibleValue)) return JSON.stringify(possibleValue);
    return possibleValue;
  }

  const operator = Operators.find(op => op.value === opName);
  return operator && operator.defaultValue ? operator.defaultValue : '';
};

export const getQueryFromUrl = (urlQuery: {
  filter: unknown;
  sort: unknown;
}) => {
  let urlFilters: string[] = [];
  if (typeof urlQuery.filter === 'string') {
    urlFilters = [urlQuery.filter];
  } else if (Array.isArray(urlQuery!.filter)) {
    urlFilters = urlQuery.filter;
  }
  const where = {
    $and: urlFilters.map(filter => {
      const parts = filter.split(';');
      const col = parts[0];
      const op = parts[1];
      const value = parts[2];
      return { [col]: { [op]: value } };
    }),
  };

  let urlSorts: string[] = [];
  if (typeof urlQuery.sort === 'string') {
    urlSorts = [urlQuery.sort];
  } else if (Array.isArray(urlQuery.sort)) {
    urlSorts = urlQuery.sort;
  }

  const order_by = urlSorts.map(sort => {
    const parts = sort.split(';');
    const column = parts[0];
    const type = parts[1];
    const nulls = 'last';
    return { column, type, nulls };
  });

  return { order_by, where };
};
