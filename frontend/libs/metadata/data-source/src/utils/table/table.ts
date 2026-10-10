import { ReactSelectOptionType } from '@hasura/shared/ui';
import { TableColumn, TableColumnTypeMap } from '../../driver/types';
import { Table } from '@hasura/shared/types';
import { getTableDisplayName } from '@hasura/shared/utils';

const escapedTableColumnRegex = /\W+|^\d/;

export function escapeTableColumnsMap(
  columns: TableColumn[],
): Record<string, string> {
  if (!columns?.length) return {};

  return columns
    .filter((col) => escapedTableColumnRegex.test(col.name))
    .reduce((acc: Record<string, string>, col) => {
      let newColName = col.name.replace(/\W+/g, '_');
      if (/^\d/.test(newColName)) newColName = `column_${newColName}`;
      acc[col.name] = newColName;

      return acc;
    }, {});
}

export function escapeTableName(tableName: string): string | null {
  if (!escapedTableColumnRegex.test(tableName)) return null;
  if (/^\d/.test(tableName)) tableName = `table_${tableName}`;

  return tableName.toLowerCase().replace(/\s+|_?\W+_?/g, '_');
}

export function getColumnDataTypeGroup(
  typeMap: TableColumnTypeMap,
  typeName: string,
): TableColumn['consoleDataType'] | undefined {
  return Object.entries(typeMap).find(([_, supportedTypes]) =>
    supportedTypes.includes(typeName),
  )?.[0] as TableColumn['consoleDataType'];
}

const getDateISOString = () => new Date().toISOString();
const getISODatePart = () => getDateISOString().slice(0, 10);
const getISOTimePart = () => getDateISOString().slice(11, 19);

export const getTableColumnValuePlaceholder = (
  group: TableColumn['consoleDataType'] | null | undefined,
  columnType: string,
) => {
  if (!group) {
    return columnType;
  }
};

export const getTableColumnDataTypePlaceholder = (
  group: TableColumn['consoleDataType'],
) => {
  switch (group) {
    case 'timestamp':
      return getDateISOString();
    case 'date':
      return getISODatePart();
    case 'time':
      // eslint-disable-next-line no-case-declarations
      const time = getISOTimePart();
      return `${time}Z or ${time}+05:30`;
    case 'json':
      return '{"name": "foo"} or [12, "bar"]';
    case 'array':
      return '{"foo", "bar"} or ["foo", "bar"]';
    case 'uuid':
      return 'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx';
    default:
      return group;
  }
};

export const isColumnAutoIncrement = (column: TableColumn): boolean => {
  return column.value_generated?.type === 'auto_increment';
};

export const createTableSelectOption = (
  table: Table,
): ReactSelectOptionType => {
  return {
    label: getTableDisplayName(table, '', ' / '),
    value: table,
  };
};
