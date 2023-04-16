import { dataSource, TableColumn } from '../../../dataSources';

const getDateISOString = () => new Date().toISOString();
const getISODatePart = () => getDateISOString().slice(0, 10);
const getISOTimePart = () => getDateISOString().slice(11, 19);

export const getPlaceholder = (type: TableColumn['data_type']) => {
  switch (type) {
    case dataSource.columnDataTypes.TIMESTAMP:
    case dataSource.columnDataTypes.DATETIME:
      return getDateISOString();
    case dataSource.columnDataTypes.DATE:
      return getISODatePart();
    case dataSource.columnDataTypes.TIME:
    case 'time':
      // eslint-disable-next-line no-case-declarations
      const time = getISOTimePart();
      return `${time}Z or ${time}+05:30`;
    case dataSource.columnDataTypes.JSONDTYPE:
    case dataSource.columnDataTypes.JSONB:
      return '{"name": "foo"} or [12, "bar"]';
    case dataSource.columnDataTypes.ARRAY:
      return '{"foo", "bar"} or ["foo", "bar"]';
    case dataSource.columnDataTypes.UUID:
      return 'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx';
    default:
      return `${type}`;
  }
};
