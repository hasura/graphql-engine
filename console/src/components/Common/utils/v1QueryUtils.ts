import { currentDriver, dataSource, terminateSql } from '../../../dataSources';
import { QualifiedTable } from '../../../metadata/types';
import { Nullable } from './tsUtils';
import { ConsoleScope } from '../../Main/ConsoleNotification';
import { BaseTableColumn } from '../../../dataSources/types';
import { sqlEscapeText } from '../../../dataSources/services/postgresql/sqlUtils';
import { FixMe } from '../../../types';
import {
  checkFeatureSupport,
  READ_ONLY_RUN_SQL_QUERIES,
} from '../../../helpers/versionUtils';

export type OrderByType = 'asc' | 'desc';
export type OrderByNulls = 'first' | 'last';

export const getRunSqlQuery = (
  sql: string,
  source: string,
  cascade = false,
  read_only = false,
  driver = currentDriver
) => {
  let type = 'run_sql';
  if (['mssql', 'bigquery', 'citus'].includes(driver)) {
    type = `${driver}_run_sql`;
  }

  return {
    type,
    args: {
      source,
      sql: terminateSql(sql),
      cascade,
      read_only: read_only && !!checkFeatureSupport(READ_ONLY_RUN_SQL_QUERIES),
    },
  };
};

export type OrderBy = {
  column: string;
  type: OrderByType;
  nulls: Nullable<OrderByNulls>;
};

export const convertPGValue = (
  value: any,
  columnInfo: BaseTableColumn
): string | number => {
  if (value === 'null' || value === null) {
    return 'null';
  }

  if (typeof value === 'string') {
    return sqlEscapeText(value);
  }

  if (
    Array.isArray(value) &&
    columnInfo.data_type !== 'json' &&
    columnInfo.data_type !== 'jsonb'
  ) {
    return `'${dataSource.arrayToPostgresArray(value)}'`;
  }

  if (typeof value === 'object') {
    return `'${JSON.stringify(value)}'`;
  }

  if (value === undefined) {
    return '';
  }

  return value;
};

export const createPKClause = (
  primaryKeyInfo: any,
  insertion: Record<string, any>,
  columns: BaseTableColumn[]
): Record<string, any> => {
  const newPKClause: Record<string, any> = {};
  const hasPrimaryKeys = primaryKeyInfo?.columns;
  if (hasPrimaryKeys) {
    primaryKeyInfo.columns.forEach((key: any) => {
      newPKClause[key] = insertion[key];
    });
  } else {
    columns.forEach(col => {
      newPKClause[col.column_name] = insertion[col.column_name];
    });
  }

  Object.keys(newPKClause).forEach(key => {
    const currentValue = newPKClause[key];
    if (Array.isArray(currentValue)) {
      newPKClause[key] = dataSource.arrayToPostgresArray(currentValue);
    }
  });

  return newPKClause;
};

export const getInsertUpQuery = (
  tableDef: FixMe,
  insertion: Record<string, any>,
  columns: BaseTableColumn[],
  source: string
) => {
  const columnValues = Object.keys(insertion)
    .map(key => `"${key}"`)
    .join(', ');

  const values = Object.values(insertion)
    .map((value, valIndex) => convertPGValue(value, columns[valIndex]))
    .join(', ');

  const sql = `INSERT INTO "${tableDef.schema}"."${tableDef.name}"(${columnValues}) VALUES (${values});`;

  return getRunSqlQuery(sql, source);
};

export const convertPGPrimaryKeyValue = (value: any, pk: string): string => {
  if (typeof value === 'string') {
    return `"${pk}" = '${value}'`;
  }

  if (Array.isArray(value)) {
    return `"${pk}" = '${dataSource.arrayToPostgresArray(value)}'`;
  }

  if (typeof value === 'object') {
    return `"${pk}" = '${JSON.stringify(value)}'`;
  }

  return `"${pk}" = ${value}`;
};

export const getInsertDownQuery = (
  tableDef: FixMe,
  insertion: Record<string, any>,
  primaryKeyInfo: any,
  columns: BaseTableColumn[],
  source: string
) => {
  const whereClause = createPKClause(primaryKeyInfo, insertion, columns);
  const clauses = Object.keys(whereClause).map(pk =>
    convertPGPrimaryKeyValue(whereClause[pk], pk)
  );
  const condition = clauses.join(' AND ');
  const sql = `DELETE FROM "${tableDef.schema}"."${tableDef.name}" WHERE ${condition};`;

  return getRunSqlQuery(sql, source);
};

type validOperators =
  | '$eq'
  | '$ne'
  | '$in'
  | '$nin'
  | '$gt'
  | '$lt'
  | '$gte'
  | '$lte';
type AndExp = Record<'$and', BoolExp[]>;
type OrExp = Record<'$or', BoolExp[]>;
type NotExp = Record<'$not', BoolExp[]>;
type ColumnExpValue = Record<validOperators | string, any>;
type ColumnExp = Record<string, ColumnExpValue>;
type BoolExp = AndExp | OrExp | NotExp | ColumnExp;

export type WhereClause = BoolExp | Record<string, any>;

export const makeOrderBy = (
  column: string,
  type: OrderByType,
  nulls: Nullable<OrderByNulls> = 'last'
): OrderBy => ({
  column,
  type,
  nulls,
});

export const getDeleteQuery = (
  pkClause: WhereClause,
  tableName: string,
  schemaName: string,
  source: string
) => {
  return {
    type: 'delete',
    args: {
      source,
      table: {
        name: tableName,
        schema: schemaName,
      },
      where: pkClause,
    },
  };
};

export const getBulkDeleteQuery = (
  pkClauses: WhereClause[],
  tableName: string,
  schemaName: string,
  source: string
) =>
  pkClauses.map(pkClause =>
    getDeleteQuery(pkClause, tableName, schemaName, source)
  );

export const getEnumOptionsQuery = (
  request: { enumTableName: string; enumColumnName: string },
  currentSchema: string,
  currentSource: string
) => ({
  type: 'select',
  args: {
    source: currentSource,
    table: {
      name: request.enumTableName,
      schema: currentSchema,
    },
    columns: [request.enumColumnName],
  },
});

export type SelectColumn = string | { name: string; columns: SelectColumn[] };

export const getSelectQuery = (
  type: 'select' | 'count',
  table: QualifiedTable,
  columns: SelectColumn[],
  where: Nullable<WhereClause>,
  offset: Nullable<number>,
  limit: Nullable<number>,
  order_by: Nullable<OrderBy[]>,
  currentDataSource?: string
) => {
  return {
    type,
    args: {
      source: currentDataSource,
      table,
      columns,
      where,
      offset,
      limit,
      order_by,
    },
  };
};

export const getConsoleNotificationQuery = (
  time: Date | string | number,
  userType?: Nullable<ConsoleScope>
) => {
  let consoleUserScopeVar = `%${userType}%`;
  if (!userType) {
    consoleUserScopeVar = '%OSS%';
  }

  const query = `query fetchNotifications($currentTime: timestamptz, $userScope: String) {
    console_notifications(
      where: {start_date: {_lte: $currentTime}, scope: {_ilike: $userScope}, _or: [{expiry_date: {_gte: $currentTime}}, {expiry_date: {_eq: null}}]},
      order_by: {priority: asc_nulls_last, start_date: desc}
    ) {
      content
      created_at
      external_link
      expiry_date
      id
      is_active
      priority
      scope
      start_date
      subject
      type
    }
  }`;

  const variables = {
    userScope: consoleUserScopeVar,
    currentTime: time,
  };

  return { query, variables };
};
