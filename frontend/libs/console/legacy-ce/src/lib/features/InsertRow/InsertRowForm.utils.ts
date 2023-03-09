import { TableColumn } from '../DataSource';

export const convertTableValue = (
  value: unknown,
  dataType: TableColumn['dataType'] | undefined
): string | number | boolean | unknown => {
  if (typeof value === 'string') {
    if (dataType === 'number') {
      return parseFloat(value);
    }

    if (dataType === 'boolean' || dataType === 'bool') {
      return value === 'true' ? true : false;
    }
  }

  return value;
};
