import {
  modifyViolationType,
  MSSqlFk,
} from '../components/Services/Data/mergeData';
import { useMetadataTables, useMetadataVersion } from '../features/MetadataAPI';
import { dataSourceSqlQueries } from '../features/SqlQueries';
import { ForeignKeyConstraint } from './../dataSources/types';
import { QualifiedTable, TableEntry } from './../metadata/types';
import { Driver } from '../dataSources';
import { QualifiedDataSource, RunSQLResponse } from './types';
import { RunSQLQueryOptions, useRunSQL } from './common';

type PartialFKConstraint = Omit<
  ForeignKeyConstraint,
  'is_table_tracked' | 'is_ref_table_tracked'
>;

type FKQueryOptions<T, N> = RunSQLQueryOptions<
  [
    name: N,
    currentDataSource: string,
    schemasOrTable: T,
    driver: string,
    version: number | undefined
  ],
  ForeignKeyConstraint[]
>;

const transformData =
  (driver: Driver, metadataTables: TableEntry[] = []) =>
  (data: RunSQLResponse): ForeignKeyConstraint[] => {
    let fkConstraints: PartialFKConstraint[] = [];
    if (driver === 'mssql') {
      const parsed: MSSqlFk[] = JSON.parse(data.result?.[1]?.[0] ?? '[]');
      fkConstraints = parsed.map(fk => {
        const mapping: Record<string, string> = {};
        fk.column_mapping.forEach(cols => {
          mapping[cols.column] = cols.referenced_column;
        });
        return {
          ...fk,
          column_mapping: mapping,
          ref_table_table_schema: fk.ref_table_schema,
          on_delete: modifyViolationType(fk.on_delete),
          on_update: modifyViolationType(fk.on_update),
        };
      });
    } else {
      fkConstraints = JSON.parse(data.result?.[1]?.[0] ?? '[]');
    }

    return fkConstraints.map(fk => ({
      ...fk,
      is_table_tracked: !!metadataTables.some(
        t =>
          t.table.name === fk.table_name && t.table.schema === fk.table_schema
      ),
      is_ref_table_tracked: !!metadataTables.some(
        t =>
          t.table.name === fk.ref_table &&
          t.table.schema === fk.ref_table_table_schema
      ),
    }));
  };

function useFKRelationshipsBase<T extends string[] | QualifiedTable, N>(
  schemasOrTable: T,
  name: N,
  dataSource: QualifiedDataSource,
  queryOptions?: FKQueryOptions<T, N>
) {
  const { source, driver } = dataSource;
  const targetDataSource = dataSourceSqlQueries[driver];
  const sql = () =>
    Array.isArray(schemasOrTable)
      ? targetDataSource.getFKRelations({ schemas: schemasOrTable })
      : targetDataSource.getFKRelations({ tables: [schemasOrTable] });

  const { data: version } = useMetadataVersion();
  const { data: metadataTables } = useMetadataTables(source);

  return useRunSQL({
    sql,
    queryKey: [name, source, schemasOrTable, driver, version],
    transformFn: transformData(driver, metadataTables),
    queryOptions,
    dataSource,
  });
}

export function useDataSourceFKRelationships(
  args: { schemas: string[] } & QualifiedDataSource,
  queryOptions?: FKQueryOptions<string[], 'dataSourceFKRelationships'>
) {
  const { schemas, ...dataSource } = args;
  return useFKRelationshipsBase(
    schemas,
    'dataSourceFKRelationships',
    dataSource,
    queryOptions
  );
}

export function useTableFKRelationships(
  args: { table: QualifiedTable } & QualifiedDataSource,
  queryOptions?: FKQueryOptions<QualifiedTable, 'tableFKRelationships'>
) {
  const { table, ...dataSource } = args;
  return useFKRelationshipsBase(
    table,
    'tableFKRelationships',
    dataSource,
    queryOptions
  );
}
