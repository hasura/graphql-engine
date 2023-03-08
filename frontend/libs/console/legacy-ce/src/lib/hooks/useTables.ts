import { useMetadataVersion, useMetadataTables } from '../features/MetadataAPI';
import { trimDefaultValue } from '../components/Services/Data/mergeData';
import { dataSourceSqlQueries } from '../features/SqlQueries';
import type { QualifiedTable, TableEntry } from './../metadata/types';
import type { NormalizedTable, TableType } from '../dataSources/types';
import type { QualifiedDataSource, RunSQLResponse } from './types';
import { Driver } from '../dataSources';
import { RunSQLQueryOptions, useRunSQL } from './common';

type TableQueryOptions<T, N, Data> = RunSQLQueryOptions<
  [
    name: N,
    currentDataSource: string,
    schemasOrTable: T,
    driver: string,
    metadataVersion: number | undefined
  ],
  Data
>;

type BigQueryMssqlTableResponse = {
  result: [string, ...string[][]];
};

function useTableBase<T extends string[] | QualifiedTable, N, D>(
  schemasOrTable: T,
  name: N,
  transformFn: (
    driver: Driver
  ) => (mt?: TableEntry[]) => (d: RunSQLResponse) => D,
  dataSource: QualifiedDataSource,
  queryOptions?: TableQueryOptions<T, N, D>
) {
  const { driver, source } = dataSource;
  const targetDataSource = dataSourceSqlQueries[driver];
  const sql = () => {
    return Array.isArray(schemasOrTable)
      ? targetDataSource.getFetchTablesListQuery({ schemas: schemasOrTable })
      : targetDataSource.getFetchTablesListQuery({ tables: [schemasOrTable] });
  };
  const { data: metadataTables, isSuccess } = useMetadataTables(source);
  const { data: version } = useMetadataVersion();
  return useRunSQL({
    sql,
    queryKey: [name, source, schemasOrTable, driver, version],
    transformFn: transformFn(driver)(metadataTables),
    queryOptions: {
      ...queryOptions,
      enabled: isSuccess && queryOptions?.enabled,
    },
    dataSource,
  });
}

const parseBigQueryMssqlTableResponse = (
  data: BigQueryMssqlTableResponse,
  driver: Driver
) => {
  const tables: Omit<NormalizedTable, 'is_table_tracked'>[] = [];
  data.result?.slice(1).forEach(row => {
    try {
      tables.push({
        table_schema: row[0],
        table_name: row[1],
        table_type: row[2] as TableType,
        comment:
          driver === 'bigquery' ? row[3] : JSON.parse(row[3])[0]?.comment,
        columns:
          driver === 'bigquery'
            ? JSON.parse(row[4])
            : JSON.parse(row[4])?.map(
                (columnData: { column_default?: string }) => {
                  if (columnData?.column_default) {
                    return {
                      ...columnData,
                      column_default: trimDefaultValue(
                        columnData.column_default
                      ),
                    };
                  }
                  return columnData;
                }
              ) ?? [],
      });
    } catch (err) {
      throw new Error(
        'Error while passing sql table result, please create an issue'
      );
    }
  });
  return tables;
};

const transformTables =
  (driver: Driver) =>
  (metadataTables: TableEntry[] = []) =>
  (data: RunSQLResponse | BigQueryMssqlTableResponse): NormalizedTable[] => {
    let tables: Omit<NormalizedTable, 'is_table_tracked'>[] = [];
    if (driver === 'bigquery' || driver === 'mssql') {
      tables = parseBigQueryMssqlTableResponse(
        data as BigQueryMssqlTableResponse,
        driver
      );
    } else {
      tables = JSON.parse(data.result?.[1]?.[0] ?? '[]');
    }
    return tables.map(table => ({
      ...table,
      is_table_tracked: metadataTables?.some(
        t =>
          t.table.name === table.table_name &&
          t.table.schema === table.table_schema
      ),
    }));
  };

const transformFindTables =
  (table: QualifiedTable) =>
  (driver: Driver) =>
  (metadataTables: TableEntry[] = []) =>
  (data: RunSQLResponse): NormalizedTable | null => {
    return (
      transformTables(driver)(metadataTables)(data).find(
        t => t.table_name === table.name && t.table_schema === table.schema
      ) ?? null
    );
  };

export function useDataSourceTables(
  args: { schemas: string[] } & QualifiedDataSource,
  queryOptions?: TableQueryOptions<
    string[],
    'dataSourceTables',
    NormalizedTable[]
  >
) {
  const { schemas, ...dataSource } = args;
  return useTableBase(
    schemas,
    'dataSourceTables',
    transformTables,
    dataSource,
    queryOptions
  );
}

export function useSingleTable(
  args: { table: QualifiedTable } & QualifiedDataSource,
  queryOptions?: TableQueryOptions<
    QualifiedTable,
    'singleTable',
    NormalizedTable | null
  >
) {
  const { table, ...dataSource } = args;
  return useTableBase(
    table,
    'singleTable',
    transformFindTables(table),
    dataSource,
    queryOptions
  );
}
