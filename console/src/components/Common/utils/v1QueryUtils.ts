import { Nullable } from './tsUtils';
import { generateTableDef, terminateSql } from '../../../dataSources';
import { QualifiedTable } from '../../../metadata/types';

export type OrderByType = 'asc' | 'desc';
export type OrderByNulls = 'first' | 'last';

export const getRunSqlQuery = (
  sql: string,
  source: string,
  shouldCascade?: boolean,
  readOnly?: boolean
) => {
  if (!sql) return {};
  return {
    type: 'run_sql',
    source,
    args: {
      sql: terminateSql(sql),
      cascade: !!shouldCascade,
      read_only: !!readOnly,
    },
  };
};

export type OrderBy = {
  column: string;
  type: OrderByType;
  nulls: Nullable<OrderByNulls>;
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
    source,
    args: {
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
  source: currentSource,
  args: {
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
    source: currentDataSource || 'default',
    args: {
      table,
      columns,
      where,
      offset,
      limit,
      order_by,
    },
  };
};

export const getFetchInvocationLogsQuery = (
  where: Nullable<WhereClause>,
  offset: Nullable<number>,
  order_by: Nullable<OrderBy[]>,
  limit: Nullable<number>
) => {
  return getSelectQuery(
    'select',
    generateTableDef('hdb_scheduled_event_invocation_logs', 'hdb_catalog'),
    ['*'],
    where,
    offset,
    limit,
    order_by
  );
};

export type SelectQueryGenerator = typeof getFetchInvocationLogsQuery;

export const getFetchManualTriggersQuery = (tableDef: QualifiedTable) =>
  getSelectQuery(
    'select',
    generateTableDef('event_triggers', 'hdb_catalog'),
    ['*'],
    {
      table_name: tableDef.name,
      schema_name: tableDef.schema,
    },
    undefined,
    undefined,
    [
      {
        column: 'name',
        type: 'asc',
        nulls: 'last',
      },
    ]
  );
