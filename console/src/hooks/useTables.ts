import React from 'react';
import Endpoints from '@/Endpoints';
import { useQuery, UseQueryOptions } from 'react-query';
import {
  mergeLoadSchemaDataPostgres,
  mergeDataCitus,
  mergeDataMssql,
  mergeDataBigQuery,
} from '@/components/Services/Data/mergeData';
import { getRunSqlQuery } from '@/components/Common/utils/v1QueryUtils';
import { Api } from './apiUtils';
import { QualifiedTable, TableEntry } from './../metadata/types';
import { useAppSelector } from './../store';
import { Table } from '../dataSources/types';
import {
  currentDriver,
  DataSourcesAPI,
  useDataSource,
  Driver,
} from '../dataSources';
import { useMetadata } from './useMetadata';
import { MetadataSelector } from './metadataSelector';

interface FetchTablesArgs {
  dataSource: DataSourcesAPI;
  options: { schemas: string[]; tables?: QualifiedTable[] };
  source: string;
  headers: Record<string, string>;
  metadataTables: TableEntry[];
  driver: Driver;
}

const mergeData = (metadataTables: TableEntry[], driver: Driver) => (
  data: {
    result: string[];
  }[]
) => {
  if (!data || !data[0] || !data[0].result) return [];

  switch (driver) {
    case 'postgres':
      return mergeLoadSchemaDataPostgres(data, metadataTables);
    case 'citus':
      return mergeDataCitus(data, metadataTables);
    case 'mssql':
      return mergeDataMssql(data, metadataTables);
    case 'bigquery':
      return mergeDataBigQuery(data, metadataTables);
    default:
      throw new Error(`Unsupported datasource: ${currentDriver}`);
  }
};

const fetchTables = (args: FetchTablesArgs) => {
  const { dataSource, options, source, headers, metadataTables, driver } = args;

  const fetchTrackedTableFkQuery = () => {
    const runSql = dataSource?.getFKRelations(options) || '';
    return getRunSqlQuery(runSql, source, false, true);
  };

  const fetchTableListQuery = () => {
    const runSql = dataSource?.getFetchTablesListQuery(options) || '';

    return getRunSqlQuery(runSql, source, false, true);
  };
  const body = {
    type: 'bulk',
    source,
    args: [
      fetchTableListQuery(),
      fetchTrackedTableFkQuery(),
      // todo: queries below could be done only when user visits `Data` page
      getRunSqlQuery(
        dataSource?.primaryKeysInfoSql(options) || '',
        source,
        false,
        true
      ),
      getRunSqlQuery(
        dataSource?.uniqueKeysSql(options) || '',
        source,
        false,
        true
      ),
    ],
  };

  if (dataSource?.checkConstraintsSql) {
    body.args.push(
      getRunSqlQuery(
        dataSource?.checkConstraintsSql(options) || '',
        source,
        false,
        true
      )
    );
  }
  return Api.post<Array<{ result: string[] }>, Table[]>(
    {
      url: Endpoints.query,
      headers,
      body,
    },
    mergeData(metadataTables, driver)
  );
};

export function useTables(
  options: { schemas: string[]; tables?: QualifiedTable[] },
  queryOptions?: Omit<
    UseQueryOptions<
      Table[],
      Error,
      Table[],
      ['tables', FetchTablesArgs['options'], number | undefined]
    >,
    'queryKey' | 'queryFn'
  >
) {
  const source: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  const headers = useAppSelector(state => state.tables.dataHeaders);

  const { data: metadataTables } = useMetadata(
    MetadataSelector.getTables(source)
  );
  const { data: version } = useMetadata(d => d.resource_version);

  const { dataSource, driver } = useDataSource();

  return useQuery({
    ...queryOptions,
    queryKey: ['tables', options, version],
    queryFn: () =>
      fetchTables({
        dataSource,
        options,
        source,
        headers,
        metadataTables: metadataTables!,
        driver,
      }),
    enabled: !!metadataTables && queryOptions?.enabled,
  });
}

//! Warning: Don't use this hook yet, use useTables above and find the target table in it
//! This is cos we need to split amount of things fetched at once. If not, this is same as calling useTables
//! and then finding the table except it is worse cos react query cache for previously fetched tables are not used
//! The plan is for this hook to work similar to useFunction hook
export function useTable(
  table: QualifiedTable,
  /* UseQueryOptions is generic over Table[] instead Table cos this hooks uses useTables under the hood */
  queryOptions?: Omit<
    UseQueryOptions<
      Table[],
      Error,
      Table[],
      ['tables', FetchTablesArgs['options'], number | undefined]
    >,
    'queryKey' | 'queryFn'
  >
) {
  const options = { schemas: [table.schema], tables: [table] };
  const response = useTables(options, queryOptions);
  return {
    ...response,
    data: React.useMemo(() => {
      return response.data?.find(
        t => t.table_name === table.name && t.table_schema === table.schema
      );
    }, [response.data, table]),
  };
}
