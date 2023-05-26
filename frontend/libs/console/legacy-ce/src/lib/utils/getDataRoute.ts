import { QualifiedFunction, Table } from '../features/hasura-metadata-types';

export const getRoute = () => ({
  connectDatabase: (driver?: string) =>
    driver
      ? `/data/v2/manage/database/add?driver=${driver}`
      : '/data/v2/manage/connect',
  database: (dataSourceName: string) =>
    encodeURI(`/data/v2/manage/database?database=${dataSourceName}`),
  table: (dataSourceName: string, table: Table, operation?: string) => {
    if (operation)
      return encodeURI(
        `/data/v2/manage/table/${operation}?database=${dataSourceName}&table=${JSON.stringify(
          table
        )}`
      );

    return encodeURI(
      `/data/v2/manage/table?database=${dataSourceName}&table=${JSON.stringify(
        table
      )}`
    );
  },
  function: (dataSourceName: string, fn: QualifiedFunction) => {
    return encodeURI(
      `/data/v2/manage/function?database=${dataSourceName}&function=${JSON.stringify(
        fn
      )}`
    );
  },
});
