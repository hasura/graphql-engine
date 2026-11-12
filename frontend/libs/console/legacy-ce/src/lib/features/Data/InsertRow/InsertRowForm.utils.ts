import { TableColumn } from '@hasura/metadata/data-source';

const numericDataTypes: TableColumn['consoleDataType'][] = [
  'integer',
  'number',
  'float',
];

/** Converts a value typed in the form to the column's generic data type. */
export const convertTableValue = (
  value: unknown,
  consoleDataType: TableColumn['consoleDataType'] | undefined,
): string | number | boolean | unknown => {
  if (typeof value === 'string' && consoleDataType) {
    if (numericDataTypes.includes(consoleDataType)) {
      return parseFloat(value);
    }

    if (consoleDataType === 'boolean') {
      return value === 'true';
    }
  }

  return value;
};
