import { Driver } from '../../../dataSources';
import type { TableORSchemaArg } from '../../../dataSources/types';
import { QualifiedTable } from '../../../metadata/types';
import { msSqlQueries } from './mssql';
import { mySqlQueries } from './mysql';
import { postgresSqlQueries } from './postgres';
import { bigquerySqlQueries } from './bigquery';
import { citusSqlQueries } from './citus';
import { cockroachDbSqlQueries } from './cockroach';

export interface DatasourceSqlQueries {
  getFetchTablesListQuery: (options: TableORSchemaArg) => string;
  primaryKeysInfoSql: (options: TableORSchemaArg) => string;
  uniqueKeysSql: (options: TableORSchemaArg) => string;
  checkConstraintsSql: (options: TableORSchemaArg) => string;
  getFKRelations: (options: TableORSchemaArg) => string;
  getTableColumnsSql: (options: QualifiedTable) => string;
}

export const dataSourceSqlQueries: Record<Driver, DatasourceSqlQueries> = {
  postgres: postgresSqlQueries,
  mysql: mySqlQueries,
  mssql: msSqlQueries,
  bigquery: bigquerySqlQueries,
  citus: citusSqlQueries,
  cockroach: cockroachDbSqlQueries,
  alloy: postgresSqlQueries,
};
