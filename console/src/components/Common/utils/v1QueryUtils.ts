import { Nullable } from './tsUtils';
import { generateTableDef, terminateSql } from '../../../dataSources';
import { QualifiedTable } from '../../../metadata/types';

export type OrderByType = 'asc' | 'desc';
export type OrderByNulls = 'first' | 'last';

export const getRunSqlQuery = (
  sql: string,
  shouldCascade?: boolean,
  readOnly?: boolean
) => {
  if (!sql) return {};
  return {
    type: 'run_sql',
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
  schemaName: string
) => {
  return {
    type: 'delete',
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
  schemaName: string
) => pkClauses.map(pkClause => getDeleteQuery(pkClause, tableName, schemaName));

export const getEnumOptionsQuery = (
  request: { enumTableName: string; enumColumnName: string },
  currentSchema: string
) => ({
  type: 'select',
  args: {
    table: {
      name: request.enumTableName,
      schema: currentSchema,
    },
    columns: [request.enumColumnName],
  },
});

export const fetchEventTriggersQuery = {
  type: 'select',
  args: {
    table: {
      name: 'event_triggers',
      schema: 'hdb_catalog',
    },
    columns: ['*'],
    order_by: [makeOrderBy('name', 'asc')],
  },
};

export const fetchScheduledTriggersQuery = {
  type: 'select',
  args: {
    table: {
      name: 'hdb_cron_triggers',
      schema: 'hdb_catalog',
    },
    columns: ['*'],
    order_by: [makeOrderBy('name', 'asc')],
  },
};

export type SelectColumn = string | { name: string; columns: SelectColumn[] };

export const getSelectQuery = (
  type: 'select' | 'count',
  table: QualifiedTable,
  columns: SelectColumn[],
  where: Nullable<WhereClause>,
  offset: Nullable<number>,
  limit: Nullable<number>,
  order_by: Nullable<OrderBy[]>
) => {
  return {
    type,
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

export const getConsoleOptsQuery = () =>
  getSelectQuery(
    'select',
    { name: 'hdb_version', schema: 'hdb_catalog' },
    ['hasura_uuid', 'console_state'],
    {},
    null,
    null,
    null
  );
