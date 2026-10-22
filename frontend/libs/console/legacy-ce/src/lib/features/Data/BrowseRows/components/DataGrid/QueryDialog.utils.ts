import { TableColumn, columnDataType } from '@hasura/metadata/data-source';

export type FilterClause = { column: string; operator: string; value?: any };

// Custom GraphQL scalars (uuid, dates, JSON, etc.) must remain strings.
// Use explicit numeric types, not "anything other than string or boolean".
// The driver's consoleDataType is checked first; these names cover drivers
// that don't classify their columns (GDC reports every column as 'string')
// and SQL types missing from a driver's classification map.
const integerTypes = new Set([
  'int',
  'integer',
  'tinyint',
  'smallint',
  'bigint',
  'int2',
  'int4',
  'int8',
  'int64',
  'serial',
  'smallserial',
  'bigserial',
  'serial2',
  'serial4',
  'serial8',
]);
// Postgres classifies bit strings as 'integer', but '101' is not the number 101.
const bitStringTypes = new Set(['bit', 'bit varying', 'varbit']);
const floatingPointTypes = new Set([
  'float',
  'float4',
  'float8',
  'float64',
  'double',
  'double precision',
  'real',
  'decimal',
  'numeric',
  'bignumeric',
  'bigdecimal',
  'money',
  'smallmoney',
  'number',
]);

type NumericKind = 'integer' | 'float';

const getNumericKind = (column: TableColumn): NumericKind | undefined => {
  if (bitStringTypes.has(columnDataType(column.dataType).toLowerCase()))
    return undefined;

  if (column.consoleDataType === 'integer') return 'integer';
  if (column.consoleDataType === 'float' || column.consoleDataType === 'number')
    return 'float';

  const dataType = columnDataType(
    column.graphQLProperties?.scalarType ?? column.dataType,
  ).toLowerCase();

  if (bitStringTypes.has(dataType)) return undefined;
  if (integerTypes.has(dataType)) return 'integer';
  if (floatingPointTypes.has(dataType)) return 'float';
  return undefined;
};

// Filters use the database column name. Only fall back to the GraphQL name,
// so a renamed column whose GraphQL name equals another column's database
// name cannot shadow it.
const findFilterColumn = (columns: TableColumn[], filter: FilterClause) =>
  columns.find((x) => x.name === filter.column) ??
  columns.find((x) => x.graphQLProperties?.name === filter.column);

const isTextColumn = (column: TableColumn) =>
  (column.consoleDataType === 'string' || column.consoleDataType === 'text') &&
  // GDC reports every column as 'string', including numeric ones.
  !getNumericKind(column);

// An empty value is a meaningful operand only for text columns (`name = ''`).
// For any other type it can't be parsed by the server, so drop the filter.
export const isIgnoredFilter = (
  columns: TableColumn[],
  filter: FilterClause,
) => {
  const isEmpty =
    filter.value === null ||
    filter.value === undefined ||
    (typeof filter.value === 'string' && filter.value.trim() === '');
  if (!isEmpty) return false;

  const column = findFilterColumn(columns, filter);
  return !!column && !isTextColumn(column);
};

export const transformFilterValues = (
  columns: TableColumn[],
  filter: FilterClause,
): FilterClause => {
  const column = findFilterColumn(columns, filter);

  // Already-typed values and operator-specific operands must not be parsed
  // according to the column's scalar type.
  if (
    !column ||
    typeof filter.value !== 'string' ||
    filter.operator === '$is_null' ||
    filter.operator === '_is_null'
  )
    return filter;

  const numericKind = getNumericKind(column);
  const value = filter.value.trim();

  if (numericKind === 'integer' && /^[+-]?\d+$/.test(value)) {
    const parsed = Number(value);
    // PostgreSQL bigint values can exceed JS's safe integer range.
    return Number.isSafeInteger(parsed) ? { ...filter, value: parsed } : filter;
  }

  if (
    numericKind === 'float' &&
    /^[+-]?(?:\d+\.?\d*|\.\d+)(?:e[+-]?\d+)?$/i.test(value)
  ) {
    const parsed = Number(value);
    return Number.isFinite(parsed) ? { ...filter, value: parsed } : filter;
  }

  return filter;
};
