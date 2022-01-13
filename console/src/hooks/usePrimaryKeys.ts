import type { MSSqlConstraint } from '@/components/Services/Data/mergeData';
import { currentDriver, Driver } from '@/dataSources';
import type { PrimaryKey } from '@/dataSources/types';
import { dataSourceSqlQueries } from '@/features/SqlQueries';
import type { QualifiedTable } from '@/metadata/types';
import { useAppSelector } from '@/store';
import { RunSQLQueryOptions, useRunSQL } from './common';
import type { RunSQLResponse } from './types';

type PKQueryOptions<T, N, D> = RunSQLQueryOptions<
  [name: N, currentDataSource: string, schemasOrTable: T],
  D
>;

const transformPK = (driver: Driver) => (
  data: RunSQLResponse
): PrimaryKey[] => {
  const parsedPKs: MSSqlConstraint[] | PrimaryKey[] = JSON.parse(
    data.result?.[1]?.[0] ?? '[]'
  );
  let primaryKeys = parsedPKs as PrimaryKey[];
  if (driver === 'mssql') {
    primaryKeys = (parsedPKs as MSSqlConstraint[]).reduce(
      (acc: PrimaryKey[], pk: MSSqlConstraint) => {
        const { table_name, table_schema, constraints } = pk;

        const columnsByConstraintName: { [name: string]: string[] } = {};
        constraints.forEach(c => {
          columnsByConstraintName[c.constraint_name] = [
            ...(columnsByConstraintName[c.constraint_name] || []),
            c.name,
          ];
        });

        const constraintInfo = Object.keys(columnsByConstraintName).map(
          pkName => ({
            table_schema,
            table_name,
            constraint_name: pkName,
            columns: columnsByConstraintName[pkName],
          })
        );
        return [...acc, ...constraintInfo];
      },
      []
    );
  }
  return primaryKeys;
};

const transformSinglePK = (table: QualifiedTable) => (driver: Driver) => (
  data: RunSQLResponse
): PrimaryKey | null => {
  const res =
    transformPK(driver)(data).find(
      key => key.table_name === table.name && key.table_schema === table.schema
    ) ?? null;
  return res;
};

function usePrimaryKeysBase<T extends string[] | QualifiedTable, N, D>(
  schemasOrTable: T,
  name: N,
  transformFn: (d: Driver) => (r: RunSQLResponse) => D,
  queryOptions?: PKQueryOptions<T, N, D>
) {
  const dataSource = dataSourceSqlQueries[currentDriver];
  const sql = () =>
    Array.isArray(schemasOrTable)
      ? dataSource.primaryKeysInfoSql({ schemas: schemasOrTable })
      : dataSource.primaryKeysInfoSql({ tables: [schemasOrTable] });
  const source: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  return useRunSQL({
    sql,
    queryKey: [name, source, schemasOrTable],
    transformFn: transformFn(currentDriver),
    queryOptions,
  });
}

export function useDataSourcePrimaryKeys(
  schemas: string[],
  queryOptions?: PKQueryOptions<string[], 'dataSourcePrimaryKeys', PrimaryKey[]>
) {
  return usePrimaryKeysBase(
    schemas,
    'dataSourcePrimaryKeys',
    transformPK,
    queryOptions
  );
}

export function useTablePrimaryKey(
  table: QualifiedTable,
  queryOptions?: PKQueryOptions<
    QualifiedTable,
    'tablePrimaryKey',
    PrimaryKey | null
  >
) {
  return usePrimaryKeysBase(
    table,
    'tablePrimaryKey',
    transformSinglePK(table),
    queryOptions
  );
}
