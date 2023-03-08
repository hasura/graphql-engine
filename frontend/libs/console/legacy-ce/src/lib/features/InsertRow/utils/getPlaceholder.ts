import { dataSource, TableColumn } from '../../../dataSources';

export const getPlaceholder = (type: TableColumn['data_type']) => {
  switch (type) {
    case dataSource.columnDataTypes.TIMESTAMP:
      return new Date().toISOString();
    case dataSource.columnDataTypes.DATE:
      return new Date().toISOString().slice(0, 10);
    case dataSource.columnDataTypes.TIME:
      const time = new Date().toISOString().slice(11, 19);
      return `${time}Z or ${time}+05:30`;
    case dataSource.columnDataTypes.JSONDTYPE:
      return '{"name": "foo"} or [12, "bar"]';
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
