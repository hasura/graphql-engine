import defaultState from '@/components/Services/Events/EventTriggers/state';
import {
  ColumnConfig,
  CustomRootFields,
  ActionDefinition,
  CustomTypes,
  QualifiedTable,
  QualifiedTableBigQuery,
  QualifiedFunction,
  RestEndpointEntry,
  RemoteSchemaDef,
  RequestTransform,
  HasuraMetadataV2,
  HasuraMetadataV3,
} from './types';
import { transformHeaders } from '../components/Common/Headers/utils';
import { LocalEventTriggerState } from '../components/Services/Events/EventTriggers/state';
import { LocalScheduledTriggerState } from '../components/Services/Events/CronTriggers/state';
import { LocalAdhocEventState } from '../components/Services/Events/AdhocEvents/Add/state';
import { RemoteRelationshipPayload } from '../components/Services/Data/TableRelationships/RemoteRelationships/utils';
import { Driver, currentDriver } from '../dataSources';
import { ConsoleState } from '../telemetry/state';
import { TriggerOperation } from '../components/Common/FilterQuery/state';
import { isEmpty } from '../components/Common/utils/jsUtils';
import { Nullable } from '../components/Common/utils/tsUtils';

export const metadataQueryTypes = [
  'add_source',
  'update_source',
  'drop_source',
  'reload_source',
  'track_table',
  'untrack_table',
  'set_table_is_enum',
  'set_apollo_federation_config',
  'track_function',
  'untrack_function',
  'create_object_relationship',
  'create_array_relationship',
  'drop_relationship',
  'set_relationship_comment',
  'rename_relationship',
  'add_computed_field',
  'drop_computed_field',
  'create_remote_relationship',
  'update_remote_relationship',
  'delete_remote_relationship',
  'create_insert_permission',
  'create_select_permission',
  'create_update_permission',
  'create_delete_permission',
  'drop_insert_permission',
  'drop_select_permission',
  'drop_update_permission',
  'drop_delete_permission',
  'set_permission_comment',
  'create_event_trigger',
  'delete_event_trigger',
  'redeliver_event',
  'invoke_event_trigger',
  'get_inconsistent_metadata',
  'drop_inconsistent_metadata',
  'add_remote_schema',
  'update_remote_schema',
  'remove_remote_schema',
  'reload_remote_schema',
  'introspect_remote_schema',
  'create_cron_trigger',
  'delete_cron_trigger',
  'create_scheduled_event',
  'add_existing_table_or_view',
  'create_query_collection',
  'drop_query_collection',
  'rename_query_collection',
  'add_query_to_collection',
  'drop_query_from_collection',
  'add_collection_to_allowlist',
  'drop_collection_from_allowlist',
  'replace_metadata',
  'export_metadata',
  'clear_metadata',
  'reload_metadata',
  'create_action',
  'drop_action',
  'update_action',
  'create_action_permission',
  'drop_action_permission',
  'set_custom_types',
  'dump_internal_state',
  'bulk',
  'get_catalog_state',
  'set_catalog_state',
  'set_table_customization',
  'get_scheduled_event_invocations',
  'get_scheduled_events',
  'delete_scheduled_event',
  'create_function_permission',
  'drop_function_permission',
  'create_rest_endpoint',
  'drop_rest_endpoint',
  'add_host_to_tls_allowlist',
  'drop_host_from_tls_allowlist',
  'dc_add_agent',
  'dc_delete_agent',
] as const;

export type MetadataQueryType = typeof metadataQueryTypes[number];

export type MetadataQueries = Record<Driver, Record<MetadataQueryType, string>>;

type MetadataQueryArgs = {
  [key: string]: any;
};

export type InvokeManualTriggerArgs = {
  name: string;
  source: string;
  payload: Record<string, any>;
};

export const getMetadataQuery = (
  type: MetadataQueryType,
  source: string,
  args: MetadataQueryArgs,
  driver: Driver = currentDriver
): {
  type: string;
  args: MetadataQueryArgs;
  version?: number;
} => {
  let prefix = '';
  switch (driver) {
    case 'mysql':
      prefix = 'mysql_';
      break;
    case 'mssql':
      prefix = 'mssql_';
      break;
    case 'bigquery':
      prefix = 'bigquery_';
      break;
    case 'citus':
      prefix = 'citus_';
      break;
    case 'cockroach':
      prefix = 'cockroach_';
      break;
    case 'postgres':
    default:
      prefix = 'pg_';
  }
  return {
    type: `${prefix}${type}`,
    args: { ...args, source },
  };
};

export type DataSourceDriver =
  | 'postgres'
  | 'mysql'
  | 'mssql'
  | 'bigquery'
  | 'citus';

export const getDataSourcePrefix = (driver: DataSourceDriver) => {
  let prefix = '';
  switch (driver) {
    case 'mysql':
      prefix = 'mysql_';
      break;
    case 'mssql':
      prefix = 'mssql_';
      break;
    case 'bigquery':
      prefix = 'bigquery_';
      break;
    case 'citus':
      prefix = 'citus_';
      break;
    case 'postgres':
    default:
      prefix = 'pg_';
  }
  return prefix;
};

export const getCreatePermissionQuery = (
  action: 'update' | 'insert' | 'delete' | 'select',
  tableDef: QualifiedTable,
  role: string,
  permission: any,
  source: string
) => {
  let queryType: MetadataQueryType;
  switch (action) {
    case 'delete':
      queryType = 'create_delete_permission';
      break;
    case 'insert':
      queryType = 'create_insert_permission';
      break;
    case 'select':
      queryType = 'create_select_permission';
      break;
    case 'update':
      queryType = 'create_update_permission';
      break;
    default:
      throw new Error('Invalid action type');
  }

  return getMetadataQuery(queryType, source, {
    table: tableDef,
    role,
    permission,
  });
};

export const getDropPermissionQuery = (
  action: string,
  tableDef: QualifiedTable,
  role: string,
  source: string
) => {
  let queryType: MetadataQueryType;
  switch (action) {
    case 'delete':
      queryType = 'drop_delete_permission';
      break;
    case 'insert':
      queryType = 'drop_insert_permission';
      break;
    case 'select':
      queryType = 'drop_select_permission';
      break;
    case 'update':
      queryType = 'drop_update_permission';
      break;
    default:
      throw new Error('Invalid action type');
  }
  return getMetadataQuery(queryType, source, {
    table: tableDef,
    role,
  });
};

export const generateSetCustomTypesQuery = (customTypes: CustomTypes) => {
  return {
    type: 'set_custom_types',
    args: customTypes,
  };
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

export const getSetCustomRootFieldsQuery = (
  tableDef: QualifiedTable,
  rootFields: CustomRootFields,
  columnConfig: ColumnConfig,
  customTableName: string | null,
  source: string
) => {
  const customNameValue = customTableName || null;

  return getMetadataQuery('set_table_customization', source, {
    source,
    table: tableDef,
    configuration: {
      custom_name: customNameValue,
      custom_root_fields: rootFields,
      column_config: columnConfig,
    },
  });
};

export const generateDropActionQuery = (name: string) => {
  return {
    type: 'drop_action',
    args: {
      name,
    },
  };
};

export const getCreateActionPermissionQuery = (
  def: { role: string; filter: Record<string, any> },
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
  actionComment: string,
  request_transform: RequestTransform
) => {
  return {
    type: 'update_action',
    args: {
      name: actionName,
      definition: def,
      comment: actionComment,
      request_transform,
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
  tableDef: QualifiedTable,
  isEnum: boolean,
  source: string
) => {
  return getMetadataQuery('set_table_is_enum', source, {
    table: tableDef,
    is_enum: isEnum,
  });
};

export const getSetTableApolloFederationQuery = (
  tableDef: QualifiedTable,
  isApolloFederationSupported: boolean,
  source: string
) => {
  return getMetadataQuery('set_apollo_federation_config', source, {
    table: tableDef,
    apollo_federation_config: isApolloFederationSupported
      ? { enable: 'v1' }
      : null,
  });
};

export const getTrackTableQuery = ({
  tableDef,
  source,
  driver,
  customColumnNames,
  customName,
}: {
  tableDef: QualifiedTable | QualifiedTableBigQuery;
  source: string;
  driver: Driver;
  customName: Nullable<string>;
  customColumnNames?: Record<string, string>;
}) => {
  const configuration: Partial<{
    column_config: ColumnConfig;
    custom_name: string;
  }> = {};
  if (!isEmpty(customColumnNames)) {
    const newColumnConfigs = Object.entries(customColumnNames || {}).map(
      ([column, columnCustomName]) => ({
        [column]: { custom_name: columnCustomName },
      })
    );
    configuration.column_config = Object.assign({}, ...newColumnConfigs);
  }
  if (customName) {
    configuration.custom_name = customName;
  }
  const args = isEmpty(configuration)
    ? { table: tableDef }
    : {
        table: tableDef,
        configuration,
      };

  return getMetadataQuery('track_table', source, args, driver);
};

export const getUntrackTableQuery = (
  tableDef: QualifiedTable,
  source: string
) => {
  return getMetadataQuery('untrack_table', source, { table: tableDef });
};

export const getAddComputedFieldQuery = (
  tableDef: QualifiedTable,
  computedFieldName: string,
  definition: any,
  comment: string,
  source: string
) => {
  return getMetadataQuery('add_computed_field', source, {
    table: tableDef,
    name: computedFieldName,
    definition,
    comment,
  });
};

export const getDropComputedFieldQuery = (
  tableDef: QualifiedTable,
  computedFieldName: string,
  source: string
) => {
  return getMetadataQuery('drop_computed_field', source, {
    table: tableDef,
    name: computedFieldName,
  });
};

export const inconsistentObjectsQuery = {
  type: 'get_inconsistent_metadata',
  args: {},
};

export const dropInconsistentObjectsQuery = {
  type: 'drop_inconsistent_metadata',
  args: {},
};

export const getReloadMetadataQuery = (
  shouldReloadRemoteSchemas: boolean | string[],
  shouldReloadSources?: boolean | string[]
) => ({
  type: 'reload_metadata',
  args: {
    reload_sources: shouldReloadSources ?? [],
    reload_remote_schemas: shouldReloadRemoteSchemas ?? [],
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
  version: 2,
  args: {},
};

export const generateReplaceMetadataQuery = (
  metadata: HasuraMetadataV3 | HasuraMetadataV2
) => ({
  type: 'replace_metadata',
  args: metadata,
});

export const resetMetadataQuery = {
  type: 'clear_metadata',
  args: {},
};

export const generateCreateEventTriggerQuery = (
  state: LocalEventTriggerState,
  source: { name: string; driver: Driver },
  replace = false,
  requestTransform?: RequestTransform
) =>
  getMetadataQuery(
    'create_event_trigger',
    source.name,
    {
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
            columns: state.isAllColumnChecked
              ? '*'
              : state.operationColumns
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
      cleanup_config: {
        ...defaultState.cleanupConfig,
        ...state.cleanupConfig,
      },
      replace,
      request_transform: requestTransform,
    },
    source.driver
  );

export const getDropEventTriggerQuery = (
  name: string,
  source: { name: string; driver: Driver }
) =>
  getMetadataQuery(
    'delete_event_trigger',
    source.name,
    {
      source: source.name,
      name: name.trim(),
    },
    source.driver
  );

export const generateCreateScheduledTriggerQuery = (
  state: LocalScheduledTriggerState,
  source: string,
  replace = false
) => ({
  type: 'create_cron_trigger',
  args: {
    source,
    name: state.name.trim(),
    webhook: state.webhook,
    schedule: state.schedule,
    payload: JSON.parse(state.payload),
    headers: transformHeaders(state.headers),
    retry_conf: {
      num_retries: state.retryConf.num_retries,
      retry_interval_seconds: state.retryConf.interval_sec,
      timeout_seconds: state.retryConf.timeout_sec,
    },
    comment: state.comment,
    include_in_metadata: state.includeInMetadata,
    replace,
  },
});

export const generateUpdateScheduledTriggerQuery = (
  state: LocalScheduledTriggerState,
  source: string
) => generateCreateScheduledTriggerQuery(state, source, true);

export const getDropScheduledTriggerQuery = (name: string, source: string) => ({
  type: 'delete_cron_trigger',
  args: {
    source,
    name: name.trim(),
  },
});

export const getCreateScheduledEventQuery = (
  state: LocalAdhocEventState,
  source: string
) => {
  return {
    type: 'create_scheduled_event',
    args: {
      source,
      webhook: state.webhook,
      schedule_at: state.time.toISOString(),
      headers: transformHeaders(state.headers),
      retry_conf: {
        num_retries: state.retryConf.num_retries,
        retry_interval_seconds: state.retryConf.interval_sec,
        timeout_seconds: state.retryConf.timeout_sec,
      },
      payload: state.payload,
      comment: state.comment,
    },
  };
};

export const getRedeliverDataEventQuery = (
  eventId: string,
  tableDef: QualifiedTable,
  source: string
) =>
  getMetadataQuery('redeliver_event', source, {
    table: tableDef,
    event_id: eventId,
  });

// this function returns the payload in old format,
// please note that there is also a new format for local table to remote schema relationship
// https://gist.github.com/0x777/e9c21e846507c6123cfb7a40c64d5772
export const getSaveRemoteRelQuery = (
  args: RemoteRelationshipPayload,
  isNew: boolean,
  source: string
) =>
  getMetadataQuery(
    isNew ? 'create_remote_relationship' : 'update_remote_relationship',
    source,
    args
  );

export const getDropRemoteRelQuery = (
  name: string,
  table: QualifiedTable,
  source: string
) =>
  getMetadataQuery('delete_remote_relationship', source, {
    name,
    table,
    source,
  });

export const getRemoteSchemaIntrospectionQuery = (
  remoteSchemaName: string
) => ({
  type: 'introspect_remote_schema',
  args: {
    name: remoteSchemaName,
  },
});

export const addExistingTableOrView = (
  tableName: string,
  schemaName: string,
  source: string
) =>
  getMetadataQuery('add_existing_table_or_view', source, {
    name: tableName,
    schema: schemaName,
  });

export const getTrackFunctionQuery = (
  name: string,
  schema: string,
  source: string,
  configuration?: Record<string, any>,
  driver?: Driver
) => {
  if (configuration) {
    return getMetadataQuery(
      'track_function',
      source,
      {
        function: { name, schema },
        configuration,
      },
      driver
    );
  }
  return getMetadataQuery(
    'track_function',
    source,
    {
      function: { name, schema },
    },
    driver
  );
};

export const getUntrackFunctionQuery = (
  name: string,
  schema: string,
  source: string
) =>
  getMetadataQuery('untrack_function', source, { function: { name, schema } });

export const getRenameRelationshipQuery = (
  table: QualifiedTable,
  name: string,
  newName: string,
  source: string
) =>
  getMetadataQuery('rename_relationship', source, {
    table,
    name,
    new_name: newName,
  });

export const getCreateObjectRelationshipQuery = (
  table: QualifiedTable,
  name: string,
  source: string
) =>
  getMetadataQuery('create_object_relationship', source, {
    name,
    table,
    using: {},
  });

export const getDropRelationshipQuery = (
  table: QualifiedTable,
  name: string,
  source: string
) =>
  getMetadataQuery('drop_relationship', source, {
    table,
    relationship: name,
  });

export const getCreateArrayRelationshipQuery = (
  table: QualifiedTable,
  name: string,
  source: string
) =>
  getMetadataQuery('create_array_relationship', source, {
    name,
    table,
    using: {},
  });

export const getSaveRemoteDbRelationshipQuery = (
  isObjRel: boolean,
  tableName: string,
  name: string,
  remoteTable: Record<string, string>,
  columnMapping: Record<string, string>,
  source: string,
  rSource: string,
  isNew: boolean,
  driver: string,
  schema: string
) => {
  const args = {
    source,
    name,
    table:
      currentDriver !== 'bigquery'
        ? { name: tableName, schema }
        : { name: tableName, dataset: schema },
    definition: {
      to_source: {
        source: rSource,
        table:
          driver !== 'bigquery'
            ? remoteTable
            : { name: remoteTable.name, dataset: remoteTable.schema },
        relationship_type: isObjRel ? 'object' : 'array',
        field_mapping: columnMapping,
      },
    },
  };

  return getMetadataQuery(
    isNew ? 'create_remote_relationship' : 'update_remote_relationship',
    source,
    args
  );
};

export const getDropRemoteDbRelationshipQuery = (
  name: string,
  tableName: string,
  source: string,
  schema: string
) =>
  getMetadataQuery('delete_remote_relationship', source, {
    name,
    table:
      currentDriver !== 'bigquery'
        ? { name: tableName, schema }
        : { name: tableName, dataset: schema },
    source,
  });

export const getAddRelationshipQuery = (
  isObjRel: boolean,
  table: QualifiedTable,
  name: string,
  remoteTable: QualifiedTable,
  columnMapping: Record<string, string>,
  source: string
) => {
  const args = {
    source,
    name,
    table,
    using: {
      manual_configuration: {
        remote_table: remoteTable,
        column_mapping: columnMapping,
      },
    },
  };
  if (isObjRel) {
    return getMetadataQuery('create_object_relationship', source, args);
  }

  return getMetadataQuery('create_array_relationship', source, args);
};

export const getSetConsoleStateQuery = (
  state: ConsoleState['console_opts']
) => ({
  type: 'set_catalog_state',
  args: {
    type: 'console',
    state,
  },
});

export const getConsoleStateQuery = {
  type: 'get_catalog_state',
  args: {},
};

export type SupportedEvents = 'cron' | 'one_off' | 'data';

export const getEventInvocationsLogByID = (
  type: SupportedEvents,
  event_id: string
) => ({
  type: 'get_scheduled_event_invocations',
  args: {
    type,
    event_id,
    get_rows_count: false,
  },
});

export const getEventInvocations = (
  type: SupportedEvents,
  limit: number,
  offset: number,
  triggerName?: string // is required for cron
) => {
  const query = {
    type: 'get_scheduled_event_invocations',
    args: {},
  };

  if (type === 'one_off') {
    query.args = {
      type,
    };
  } else {
    query.args = {
      type,
      trigger_name: triggerName,
    };
  }

  return {
    ...query,
    args: {
      ...query.args,
      limit,
      offset,
      get_rows_count: false,
    },
  };
};

export const getScheduledEvents = (
  type: SupportedEvents,
  limit: number,
  offset: number,
  triggerOp: Exclude<TriggerOperation, 'invocation'>,
  triggerName?: string // is required for cron triggers
) => {
  const query = {
    type: 'get_scheduled_events',
    args: {},
  };
  const statusPending = ['scheduled'];
  const statusProcessed = ['delivered', 'dead', 'error'];

  if (type === 'one_off') {
    query.args = {
      type,
    };
  } else {
    query.args = {
      type,
      trigger_name: triggerName,
    };
  }

  if (triggerOp === 'pending') {
    query.args = {
      ...query.args,
      status: statusPending,
    };
  } else {
    query.args = {
      ...query.args,
      status: statusProcessed,
    };
  }

  return {
    ...query,
    args: {
      ...query.args,
      limit,
      offset,
      get_rows_count: false,
    },
  };
};

export const deleteScheduledEvent = (
  type: SupportedEvents,
  event_id: string
) => ({
  type: 'delete_scheduled_event',
  args: {
    type,
    event_id,
  },
});

export const invokeManualTriggerQuery = (
  args: InvokeManualTriggerArgs,
  source: string
) => getMetadataQuery('invoke_event_trigger', source, args);

export const createFunctionPermissionQuery = (
  source: string,
  func: QualifiedFunction,
  role: string
) =>
  getMetadataQuery('create_function_permission', source, {
    function: func,
    role,
  });

export const dropFunctionPermissionQuery = (
  source: string,
  func: QualifiedFunction,
  role: string
) =>
  getMetadataQuery('drop_function_permission', source, {
    function: func,
    role,
  });

export const createRESTEndpointQuery = (args: RestEndpointEntry) => ({
  type: 'create_rest_endpoint',
  args,
});

export const dropRESTEndpointQuery = (name: string) => ({
  type: 'drop_rest_endpoint',
  args: { name },
});

const getMetadataQueryForRemoteSchema =
  (queryName: 'add' | 'update') =>
  (name: string, definition: RemoteSchemaDef, comment?: string) => ({
    type: `${queryName}_remote_schema` as MetadataQueryType,
    args: {
      name,
      definition,
      comment: comment ?? null,
    },
  });

export const addRemoteSchemaQuery = getMetadataQueryForRemoteSchema('add');

export const updateRemoteSchemaQuery =
  getMetadataQueryForRemoteSchema('update');

export const removeRemoteSchemaQuery = (name: string) => ({
  type: 'remove_remote_schema',
  args: { name },
});
