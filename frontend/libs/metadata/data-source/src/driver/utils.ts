import { SupportedDriver } from '@hasura/shared/types';
import { TableColumn } from './types';
import { capitalize } from 'inflection';

export function columnDataType(dataType: TableColumn['dataType']): string {
  if (typeof dataType === 'string') {
    return dataType;
  }

  return dataType.type;
}

const MIGRATION_NAME_REGEX = /[^\w]/g;

export function sanitizeMigrationName(...parts: string[]): string {
  return parts
    .map((p) => p.trim().replaceAll(MIGRATION_NAME_REGEX, '_'))
    .join('_');
}

export const terminateSql = (sql: string) => {
  const sqlSanitised = sql.trim();
  return sqlSanitised[sqlSanitised.length - 1] !== ';'
    ? `${sqlSanitised};`
    : sqlSanitised;
};

export function getDriverLabel(driver: SupportedDriver): string {
  switch (driver) {
    case 'postgres':
      return 'PostgreSQL';
    case 'mysql':
      return 'MySQL';
    case 'alloy':
      return 'AlloyDB';
    case 'mssql':
      return 'MS SQL Server';
    case 'bigquery':
      return 'BigQuery';
    case 'citus':
      return 'Citus';
    case 'athena':
      return 'Amazon Athena';
    case 'cockroach':
      return 'CockroachDB';
    case 'mariadb':
      return 'MariaDB';
    case 'mongodb':
      return 'MongoDB';
    case 'oracle':
      return 'Oracle';
    case 'redshift':
      return 'Redshift';
    case 'snowflake':
      return 'Snowflake';
    case 'sqlite':
      return 'SQLite';
    default:
      return capitalize(driver);
  }
}
