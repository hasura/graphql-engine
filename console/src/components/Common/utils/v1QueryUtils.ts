import { terminateSql } from './sqlUtils';
import { LocalScheduledTriggerState } from '../../Services/Events/CronTriggers/state';
import { LocalAdhocEventState } from '../../Services/Events/AdhocEvents/Add/state';
import { LocalEventTriggerState } from '../../Services/Events/EventTriggers/state';
import { RemoteRelationshipPayload } from '../../Services/Data/TableRelationships/RemoteRelationships/utils';
import { transformHeaders } from '../Headers/utils';
import {
  generateTableDef,
  BaseTableColumn,
  PrimaryKey,
  createPKClause,
  arrayToPostgresArray,
} from './pgUtils';
import { Nullable } from './tsUtils';
import { ConsoleState } from '../../../types';
import { ConsoleScope } from '../../Main/ConsoleNotification';

export type OrderByType = 'asc' | 'desc';
export type OrderByNulls = 'first' | 'last';

export type OrderBy = {
  column: string;
  type: OrderByType;
  nulls: Nullable<OrderByNulls>;
};
export const makeOrderBy = (
  column: string,
  type: OrderByType,
  nulls: Nullable<OrderByNulls> = 'last'
): OrderBy => ({
  column,
  type,
  nulls,
});

export type WhereClause = any;

export type TableDefinition = {
  name: string;
  schema: string;
};

export type FunctionDefinition = {
  name: string;
  schema: string;
};

type GraphQLArgument = {
  name: string;
  type: string;
  description: string;
};

export type Header = {
  name: string;
  value?: string;
  value_from_env?: string;
};

export const getRunSqlQuery = (
  sql: string,
  cascade = false,
  read_only = false
) => {
  return {
    type: 'run_sql',
    args: {
      sql: terminateSql(sql),
      cascade,
      read_only,
    },
  };
};

export const getCreatePermissionQuery = (
  action: string,
  tableDef: TableDefinition,
  role: string,
  permission: any
) => {
  return {
    type: `create_${action}_permission`,
    args: {
      table: tableDef,
      role,
      permission,
    },
  };
};

export const getDropPermissionQuery = (
  action: string,
  tableDef: TableDefinition,
  role: string
) => {
  return {
    type: `drop_${action}_permission`,
    args: {
      table: tableDef,
      role,
    },
  };
};

export type PGReturnValueType =
  | 'null'
  | null
  | string
  | unknown[]
  | undefined
  | Record<string, any>
  | number;

export const convertPGValue = (
  value: PGReturnValueType,
  columnInfo: BaseTableColumn
): string | number => {
  if (value === 'null' || value === null) {
    return 'null';
  }

  if (typeof value === 'string') {
    return `'${value}'`;
  }

  if (
    Array.isArray(value) &&
    columnInfo.data_type !== 'json' &&
    columnInfo.data_type !== 'jsonb'
  ) {
    return `'${arrayToPostgresArray(value)}'`;
  }

  if (typeof value === 'object') {
    return `'${JSON.stringify(value)}'`;
  }

  if (value === undefined) {
    return '';
  }

  return value;
};

export const getInsertUpQuery = (
  tableDef: TableDefinition,
  insertion: Record<string, PGReturnValueType>,
  columns: BaseTableColumn[]
) => {
  const columnValues = Object.keys(insertion)
    .map(key => `"${key}"`)
    .join(', ');

  const values = Object.values(insertion)
    .map((value, valIndex) => convertPGValue(value, columns[valIndex]))
    .join(', ');

  const sql = `INSERT INTO "${tableDef.schema}"."${tableDef.name}"(${columnValues}) VALUES (${values});`;

  return getRunSqlQuery(sql);
};

export const convertPGPrimaryKeyValue = (
  value: PGReturnValueType,
  pk: string
): string => {
  if (typeof value === 'string') {
    return `"${pk}" = '${value}'`;
  }

  if (Array.isArray(value)) {
    return `"${pk}" = '${arrayToPostgresArray(value)}'`;
  }

  if (typeof value === 'object') {
    return `"${pk}" = '${JSON.stringify(value)}'`;
  }

  return `"${pk}" = ${value}`;
};

export const getInsertDownQuery = (
  tableDef: TableDefinition,
  insertion: Record<string, PGReturnValueType>,
  primaryKeyInfo: PrimaryKey,
  columns: BaseTableColumn[]
) => {
  const whereClause = createPKClause(primaryKeyInfo, insertion, columns);
  const clauses = Object.keys(whereClause).map(pk =>
    convertPGPrimaryKeyValue(whereClause[pk], pk)
  );
  const condition = clauses.join(' AND ');
  const sql = `DELETE FROM "${tableDef.schema}"."${tableDef.name}" WHERE ${condition};`;

  return getRunSqlQuery(sql);
};

type CustomTypeScalar = {
  name: string;
  description: string;
};

type CustomTypeEnumValue = {
  value: string;
  description: string;
};
type CustomTypeEnum = {
  name: string;
  values: CustomTypeEnumValue[];
  description: string;
};

type CustomTypeObjectField = {
  name: string;
  type: string;
  description: string;
};
type CustomTypeObject = {
  name: string;
  description: string;
  fields: CustomTypeObjectField[];
};

type CustomTypeInputObjectField = {
  name: string;
  type: string;
  description: string;
};
type CustomTypeInputObject = {
  name: string;
  description: string;
  fields: CustomTypeInputObjectField[];
};

type CustomTypes = {
  scalars: CustomTypeScalar[];
  enums: CustomTypeEnum[];
  objects: CustomTypeObject[];
  input_objects: CustomTypeInputObject[];
};

export const generateSetCustomTypesQuery = (customTypes: CustomTypes) => {
  return {
    type: 'set_custom_types',
    args: customTypes,
  };
};

type ActionDefinition = {
  arguments: GraphQLArgument[];
  kind: 'synchronous' | 'asynchronous';
  output_type: string;
  handler: string;
  headers: Header[];
  forward_client_headers: boolean;
};

export const generateCreateActionQuery = (
  name: string,
  definition: ActionDefinition,
  comment: string
) => {
  return {
    type: 'create_action',
    args: {
      name,
      definition,
      comment,
    },
  };
};

export const generateDropActionQuery = (name: string) => {
  return {
    type: 'drop_action',
    args: {
      name,
    },
  };
};

export const getFetchActionsQuery = () => {
  return {
    type: 'select',
    args: {
      table: {
        name: 'hdb_action',
        schema: 'hdb_catalog',
      },
      columns: ['*.*'],
      order_by: [{ column: 'action_name', type: 'asc' }],
    },
  };
};

export const getFetchCustomTypesQuery = () => {
  return {
    type: 'select',
    args: {
      table: {
        name: 'hdb_custom_types',
        schema: 'hdb_catalog',
      },
      columns: ['*.*'],
    },
  };
};

export type CustomRootFields = {
  select: string;
  select_by_pk: string;
  select_aggregate: string;
  insert: string;
  update: string;
  delete: string;
};

type CustomColumnNames = {
  [columnName: string]: string;
};

export const getSetCustomRootFieldsQuery = (
  tableDef: TableDefinition,
  rootFields: CustomRootFields,
  customColumnNames: CustomColumnNames
) => {
  return {
    type: 'set_table_custom_fields',
    version: 2,
    args: {
      table: tableDef,
      custom_root_fields: rootFields,
      custom_column_names: customColumnNames,
    },
  };
};

export const getFetchAllRolesQuery = () => ({
  type: 'select',
  args: {
    table: {
      schema: 'hdb_catalog',
      name: 'hdb_role',
    },
    columns: ['role_name'],
    order_by: [{ column: 'role_name', type: 'asc' }],
  },
});

// TODO Refactor and accept role, filter and action name
export const getCreateActionPermissionQuery = (
  def: { role: string; filter: any },
  actionName: string
) => {
  return {
    type: 'create_action_permission',
    args: {
      action: actionName,
      role: def.role,
      definition: {
        select: {
          filter: def.filter,
        },
      },
    },
  };
};

export const getUpdateActionQuery = (
  def: ActionDefinition,
  actionName: string,
  actionComment: string
) => {
  return {
    type: 'update_action',
    args: {
      name: actionName,
      definition: def,
      comment: actionComment,
    },
  };
};

export const getDropActionPermissionQuery = (
  role: string,
  actionName: string
) => {
  return {
    type: 'drop_action_permission',
    args: {
      action: actionName,
      role,
    },
  };
};

export const getSetTableEnumQuery = (
  tableDef: TableDefinition,
  isEnum: boolean
) => {
  return {
    type: 'set_table_is_enum',
    args: {
      table: tableDef,
      is_enum: isEnum,
    },
  };
};

export const getTrackTableQuery = (tableDef: TableDefinition) => {
  return {
    type: 'add_existing_table_or_view',
    args: tableDef,
  };
};

export const getUntrackTableQuery = (tableDef: TableDefinition) => {
  return {
    type: 'untrack_table',
    args: {
      table: tableDef,
    },
  };
};

export const getAddComputedFieldQuery = (
  tableDef: TableDefinition,
  computedFieldName: string,
  definition: any, // TODO
  comment: string
) => {
  return {
    type: 'add_computed_field',
    args: {
      table: tableDef,
      name: computedFieldName,
      definition: {
        ...definition,
      },
      comment,
    },
  };
};

export const getDropComputedFieldQuery = (
  tableDef: TableDefinition,
  computedFieldName: string
) => {
  return {
    type: 'drop_computed_field',
    args: {
      table: tableDef,
      name: computedFieldName,
    },
  };
};

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
  pkClauses: WhereClause,
  tableName: string,
  schemaName: string
) =>
  pkClauses.map((pkClause: WhereClause) =>
    getDeleteQuery(pkClause, tableName, schemaName)
  );

export const getEnumOptionsQuery = (
  request: { enumTableName: string; enumColumnName: string },
  currentSchema: string
) => {
  return {
    type: 'select',
    args: {
      table: {
        name: request.enumTableName,
        schema: currentSchema,
      },
      columns: [request.enumColumnName],
    },
  };
};

export const inconsistentObjectsQuery = {
  type: 'get_inconsistent_metadata',
  args: {},
};

export const dropInconsistentObjectsQuery = {
  type: 'drop_inconsistent_metadata',
  args: {},
};

export const getReloadMetadataQuery = (shouldReloadRemoteSchemas: boolean) => ({
  type: 'reload_metadata',
  args: {
    reload_remote_schemas: shouldReloadRemoteSchemas,
  },
});

export const getReloadRemoteSchemaCacheQuery = (remoteSchemaName: string) => {
  return {
    type: 'reload_remote_schema',
    args: {
      name: remoteSchemaName,
    },
  };
};

export const exportMetadataQuery = {
  type: 'export_metadata',
  args: {},
};

// type the metadata
export const generateReplaceMetadataQuery = (metadataJson: any) => ({
  type: 'replace_metadata',
  args: metadataJson,
});

export const resetMetadataQuery = {
  type: 'clear_metadata',
  args: {},
};

export const fetchEventTriggersQuery = {
  type: 'select',
  args: {
    table: {
      name: 'event_triggers',
      schema: 'hdb_catalog',
    },
    columns: ['*'],
    order_by: [{ column: 'name', type: 'asc' }],
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
    order_by: [{ column: 'name', type: 'asc' }],
  },
};

export const getBulkQuery = (args: any[]) => {
  return {
    type: 'bulk',
    args,
  };
};

export const generateCreateEventTriggerQuery = (
  state: LocalEventTriggerState,
  replace = false
) => {
  return {
    type: 'create_event_trigger',
    args: {
      name: state.name.trim(),
      table: state.table,
      webhook:
        state.webhook.type === 'static' ? state.webhook.value.trim() : null,
      webhook_from_env:
        state.webhook.type === 'env' ? state.webhook.value.trim() : null,
      insert: state.operations.insert
        ? {
            columns: '*',
          }
        : null,
      update: state.operations.update
        ? {
            columns: state.operationColumns
              .filter(c => !!c.enabled)
              .map(c => c.name),
          }
        : null,
      delete: state.operations.delete
        ? {
            columns: '*',
          }
        : null,
      enable_manual: state.operations.enable_manual,
      retry_conf: state.retryConf,
      headers: transformHeaders(state.headers),
      replace,
    },
  };
};

export const getDropEventTriggerQuery = (name: string) => ({
  type: 'delete_event_trigger',
  args: {
    name: name.trim(),
  },
});

export const generateCreateScheduledTriggerQuery = (
  state: LocalScheduledTriggerState,
  replace = false
) => ({
  type: 'create_cron_trigger',
  args: {
    name: state.name.trim(),
    webhook: state.webhook,
    schedule: state.schedule,
    payload: JSON.parse(state.payload),
    headers: transformHeaders(state.headers),
    retry_conf: {
      num_retries: state.retryConf.num_retries,
      retry_interval_seconds: state.retryConf.interval_sec,
      timeout_seconds: state.retryConf.timeout_sec,
      tolerance_seconds: state.retryConf.tolerance_sec,
    },
    comment: state.comment,
    include_in_metadata: state.includeInMetadata,
    replace,
  },
});

export const generateUpdateScheduledTriggerQuery = (
  state: LocalScheduledTriggerState
) => generateCreateScheduledTriggerQuery(state, true);

export const getDropScheduledTriggerQuery = (name: string) => ({
  type: 'delete_cron_trigger',
  args: {
    name: name.trim(),
  },
});

export const getCreateScheduledEventQuery = (state: LocalAdhocEventState) => {
  return {
    type: 'create_scheduled_event',
    args: {
      webhook: state.webhook,
      schedule_at: state.time.toISOString(),
      headers: transformHeaders(state.headers),
      retry_conf: {
        num_retries: state.retryConf.num_retries,
        retry_interval_seconds: state.retryConf.interval_sec,
        timeout_seconds: state.retryConf.timeout_sec,
        tolerance_seconds: state.retryConf.tolerance_sec,
      },
      payload: state.payload,
      comment: state.comment,
    },
  };
};

export type SelectColumn = string | { name: string; columns: SelectColumn[] };

export const getSelectQuery = (
  type: 'select' | 'count',
  table: TableDefinition,
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

export const getFetchManualTriggersQuery = (tableDef: TableDefinition) =>
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

export const getRedeliverDataEventQuery = (eventId: string) => ({
  type: 'redeliver_event',
  args: {
    event_id: eventId,
  },
});

export const getSaveRemoteRelQuery = (
  args: RemoteRelationshipPayload,
  isNew: boolean
) => ({
  type: `${isNew ? 'create' : 'update'}_remote_relationship`,
  args,
});

export const getDropRemoteRelQuery = (
  name: string,
  table: TableDefinition
) => ({
  type: 'delete_remote_relationship',
  args: {
    name,
    table,
  },
});

export const getRemoteSchemaIntrospectionQuery = (
  remoteSchemaName: string
) => ({
  type: 'introspect_remote_schema',
  args: {
    name: remoteSchemaName,
  },
});

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

export const getUpdateConsoleStateQuery = (
  updatedConsoleState: ConsoleState['console_opts']
) => {
  return {
    type: 'update',
    args: {
      table: { name: 'hdb_version', schema: 'hdb_catalog' },
      $set: { console_state: updatedConsoleState },
      where: {},
      returning: ['console_state'],
    },
  };
};

export const getConsoleNotificationQuery = (
  time: Date | string | number,
  userType?: Nullable<ConsoleScope>
) => {
  let consoleUserScope = {
    $ilike: `%${userType}%`,
  };
  if (!userType) {
    consoleUserScope = {
      $ilike: '%OSS%',
    };
  }

  return {
    args: {
      table: 'console_notification',
      columns: ['*'],
      where: {
        $or: [
          {
            expiry_date: {
              $gte: time,
            },
          },
          {
            expiry_date: {
              $eq: null,
            },
          },
        ],
        scope: consoleUserScope,
        start_date: { $lte: time },
      },
      order_by: [
        {
          type: 'asc',
          nulls: 'last',
          column: 'priority',
        },
        {
          type: 'desc',
          column: 'start_date',
        },
      ],
    },
    type: 'select',
  };
};
