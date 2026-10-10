import { SupportedDriver } from '@hasura/shared/types';
import { TableColumn, TableColumnTypeMap } from './types';
import { capitalize } from 'inflection';

export function isDataType(
  typeMap: TableColumnTypeMap,
  columnDataType: TableColumn['dataType'],
  expectedType: TableColumn['consoleDataType'],
): boolean {
  const dataType =
    typeof columnDataType === 'string' ? columnDataType : columnDataType.type;
  switch (expectedType) {
    case 'boolean':
      return typeMap.boolean.includes(dataType);
    case 'array':
      return typeMap.array.includes(dataType);
    case 'date':
      return typeMap.date.includes(dataType);
    case 'float':
      return typeMap.float.includes(dataType);
    case 'geography':
      return typeMap.geography.includes(dataType);
    case 'integer':
      return typeMap.integer.includes(dataType);
    case 'number':
      return (
        typeMap.number.includes(dataType) ||
        typeMap.float.includes(dataType) ||
        typeMap.integer.includes(dataType)
      );
    case 'time':
      return typeMap.time.includes(dataType);
    case 'uuid':
      return typeMap.uuid.includes(dataType);
    case 'json':
      return typeMap.json.includes(dataType);
    case 'timestamp':
      return typeMap.timestamp.includes(dataType);
    case 'text':
    case 'string':
      return (
        typeMap.string.includes(dataType) || typeMap.text.includes(dataType)
      );
    default:
      return false;
  }
}

export function columnDataType(dataType: TableColumn['dataType']): string {
  if (typeof dataType === 'string') {
    return dataType;
  }

  return dataType.type;
}

export const sqlEscapeText = (rawText: string) => {
  let text = rawText;

  if (text) {
    text = text.replace(/'/g, "\\'");
  }

  return `E'${text}'`;
};

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
