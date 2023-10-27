import { TableColumn } from '../DataSource';
import { columnDataType } from '../DataSource/utils';

const numericDataTypes = ['number', 'float', 'integer', 'int', 'Int'];

export const convertTableValue = (
  value: unknown,
  dataType: TableColumn['dataType'] | undefined
): string | number | boolean | unknown => {
  if (typeof value === 'string') {
    if (dataType && numericDataTypes.includes(columnDataType(dataType))) {
      return parseFloat(value);
    }

    if (dataType === 'boolean' || dataType === 'bool') {
      return value === 'true' ? true : false;
    }
  }

  return value;
};
