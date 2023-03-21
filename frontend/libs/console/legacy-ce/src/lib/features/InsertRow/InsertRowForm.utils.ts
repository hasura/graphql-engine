import { TableColumn } from '../DataSource';

const numericDataTypes = ['number', 'float', 'integer'];

export const convertTableValue = (
  value: unknown,
  dataType: TableColumn['dataType'] | undefined
): string | number | boolean | unknown => {
  if (typeof value === 'string') {
    if (dataType && numericDataTypes.includes(dataType)) {
      return parseFloat(value);
    }

    if (dataType === 'boolean' || dataType === 'bool') {
      return value === 'true' ? true : false;
    }
  }

  return value;
};
