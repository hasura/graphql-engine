import { currentDriver, terminateSql } from '../../../dataSources';
import { NativeDrivers } from '../../../features/hasura-metadata-types';
import { isPostgres } from '../../../metadata/dataSource.utils';

type AllowedRunSQLKeys =
  | 'mssql_run_sql'
  | 'bigquery_run_sql'
  | 'citus_run_sql'
  | 'mysql_run_sql'
  | 'run_sql'
  | 'cockroach_run_sql';

type CustomSqlTypeDrivers = Exclude<NativeDrivers, 'postgres' | 'alloy'>;

type GetRunSqlQueryOptions = {
  sql: string;
  source: string;
  cascade?: boolean;
  read_only?: boolean;
  driver?: NativeDrivers;
};

type GetRunSqlQueryReturn = {
  type: AllowedRunSQLKeys;
  args: {
    source: string;
    sql: string;
    cascade: boolean;
    read_only: boolean;
  };
};

export const getRunSqlType = (driver: NativeDrivers): AllowedRunSQLKeys => {
  if (isPostgres(driver)) {
    return 'run_sql';
  }

  return `${driver as CustomSqlTypeDrivers}_run_sql`;
};

//overload: params as separate arguments
export function getRunSqlQuery(
  sql: string,
  source: string,
  cascade?: boolean,
  read_only?: boolean,
  driver?: NativeDrivers
): GetRunSqlQueryReturn;

//overload: params as a single object
export function getRunSqlQuery(
  options: GetRunSqlQueryOptions
): GetRunSqlQueryReturn;

//signature
export function getRunSqlQuery(
  sqlOrOptions: string | GetRunSqlQueryOptions,
  source?: string,
  cascade = false,
  read_only = false,
  driver = currentDriver
) {
  if (typeof sqlOrOptions === 'string') {
    return execute.withMultipleArgs(
      sqlOrOptions,
      source ?? '',
      cascade,
      read_only,
      driver
    );
  } else {
    return execute.withSingleArg(sqlOrOptions);
  }
}

const execute = {
  withMultipleArgs: (
    sql: string,
    source: string,
    cascade = false,
    read_only = false,
    driver = currentDriver
  ): GetRunSqlQueryReturn => ({
    type: getRunSqlType(driver),
    args: {
      source,
      sql: terminateSql(sql),
      cascade,
      read_only,
    },
  }),
  withSingleArg: ({
    sql,
    source,
    cascade = false,
    read_only = false,
    driver = currentDriver,
  }: GetRunSqlQueryOptions) => ({
    type: getRunSqlType(driver),
    args: {
      source,
      sql: terminateSql(sql),
      cascade,
      read_only,
    },
  }),
};
