import { terminateSql } from './sqlUtils';
import { generateTableDef } from './pgUtils';

// TODO add type for the where clause

// TODO extend all queries with v1 query type

type SelectColumn = string | { name: string; columns: SelectColumn[] };

type WhereClause = any;

type OrderBy = {
  column: string;
  type: 'asc' | 'desc';
  nulls?: string;
};

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
  shouldCascade: boolean,
  readOnly: boolean
) => {
  return {
    type: 'run_sql',
    args: {
      sql: terminateSql(sql),
      cascade: !!shouldCascade,
      read_only: !!readOnly,
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

type CustomRootFields = {
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
    columns: ['*.*'],
    order_by: [{ column: 'name', type: 'asc' }],
  },
};

export const fetchScheduledTriggersQuery = {
  type: 'select',
  args: {
    table: {
      name: 'hdb_scheduled_trigger',
      schema: 'hdb_catalog',
    },
    columns: ['*.*'],
    order_by: [{ column: 'name', type: 'asc' }],
  },
};

export const getBulkQuery = (args: any[]) => {
  return {
    type: 'bulk',
    args,
  };
};

export const getSelectQuery = (
  type: 'count' | 'select',
  table: TableDefinition,
  columns: SelectColumn[],
  where?: WhereClause,
  offset?: number,
  limit?: number,
  order_by?: OrderBy[]
) => {
  return {
    type: 'select',
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

export const getFetchScheduledEventsInvocationLogsQuery = (
  where?: WhereClause,
  offset?: number,
  order_by?: OrderBy[],
  limit?: number
) => {
  return getSelectQuery(
    'select',
    generateTableDef('hdb_scheduled_event_invocation_logs', 'hdb_catalog'),
    ['*'],
    where,
    undefined,
    limit,
    order_by
  );
};
export type SelectAllQueryGenerator = typeof getFetchScheduledEventsInvocationLogsQuery;

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
        type: 'asc',
        column: 'name',
        nulls: 'last',
      },
    ]
  );
