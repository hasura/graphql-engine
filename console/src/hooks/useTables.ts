import { useMetadataVersion, useMetadataTables } from '@/features/MetadataAPI';
import { trimDefaultValue } from '@/components/Services/Data/mergeData';
import { dataSourceSqlQueries } from '@/features/SqlQueries';
import type { QualifiedTable, TableEntry } from './../metadata/types';
import { useAppSelector } from './../store';
import type { NormalizedTable, TableType } from '../dataSources/types';
import type { RunSQLResponse } from './types';
import { currentDriver, Driver } from '../dataSources';
import { RunSQLQueryOptions, useRunSQL } from './common';

type TableQueryOptions<T, N, Data> = RunSQLQueryOptions<
  [
    name: N,
    currentDataSource: string,
    schemasOrTable: T,
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
  queryOptions?: TableQueryOptions<T, N, D>
) {
  const dataSource = dataSourceSqlQueries[currentDriver];
  const sql = () => {
    return Array.isArray(schemasOrTable)
      ? dataSource.getFetchTablesListQuery({ schemas: schemasOrTable })
      : dataSource.getFetchTablesListQuery({ tables: [schemasOrTable] });
  };
  const source: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  const { data: metadataTables, isSuccess } = useMetadataTables();
  const { data: version } = useMetadataVersion();
  return useRunSQL({
    sql,
    queryKey: [name, source, schemasOrTable, version],
    transformFn: transformFn(currentDriver)(metadataTables),
    queryOptions: {
      ...queryOptions,
      enabled: isSuccess && queryOptions?.enabled,
    },
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

const transformTables = (driver: Driver) => (
  metadataTables: TableEntry[] = []
) => (data: RunSQLResponse | BigQueryMssqlTableResponse): NormalizedTable[] => {
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

const transformFindTables = (table: QualifiedTable) => (driver: Driver) => (
  metadataTables: TableEntry[] = []
) => (data: RunSQLResponse): NormalizedTable | null => {
  return (
    transformTables(driver)(metadataTables)(data).find(
      t => t.table_name === table.name && t.table_schema === table.schema
    ) ?? null
  );
};

export function useDataSourceTables(
  schemas: string[],
  queryOptions?: TableQueryOptions<
    string[],
    'dataSourceTables',
    NormalizedTable[]
  >
) {
  return useTableBase(
    schemas,
    'dataSourceTables',
    transformTables,
    queryOptions
  );
}

export function useSingleTable(
  table: QualifiedTable,
  queryOptions?: TableQueryOptions<
    QualifiedTable,
    'singleTable',
    NormalizedTable | null
  >
) {
  return useTableBase(
    table,
    'singleTable',
    transformFindTables(table),
    queryOptions
  );
}
