// Note: This file MUST contain all/only metadata requests

import {
  CustomRootFields,
  ActionDefinition,
  CustomTypes,
  HasuraMetadataV2,
  QualifiedTable,
} from './types';
import { transformHeaders } from '../components/Common/Headers/utils';
import { LocalEventTriggerState } from '../components/Services/Events/EventTriggers/state';
import { LocalScheduledTriggerState } from '../components/Services/Events/CronTriggers/state';
import { LocalAdhocEventState } from '../components/Services/Events/AdhocEvents/Add/state';
import { RemoteRelationshipPayload } from '../components/Services/Data/TableRelationships/RemoteRelationships/utils';
import { Driver } from '../dataSources';

export const metadataQueryTypes = [
  'add_existing_table_or_view',
  'track_table',
  'untrack_table',
  'set_table_is_enum',
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
  'get_inconsistent_metadata',
  'drop_inconsistent_metadata',
  'add_remote_schema',
  'remove_remote_schema',
  'reload_remote_schema',
  'introspect_remote_schema',
  'create_event_trigger',
  'delete_event_trigger',
  'redeliver_event',
  'invoke_event_trigger',
  'create_cron_trigger',
  'delete_cron_trigger',
  'create_scheduled_event',
  'create_query_collection',
  'drop_query_collection',
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
  'dump_internal_state',
  'set_custom_types',
];

export type MetadataQueryType =
  | 'create_select_permission'
  | 'create_update_permission'
  | 'create_delete_permission'
  | 'create_insert_permission'
  | 'drop_select_permission'
  | 'drop_update_permission'
  | 'drop_delete_permission'
  | 'drop_insert_permission'
  | 'set_custom_types'
  | 'create_action'
  | 'set_table_custom_fields'
  | 'drop_action'
  | 'create_action_permission'
  | 'update_action'
  | 'drop_action_permission'
  | 'set_table_is_enum'
  | 'add_existing_table_or_view'
  | 'untrack_table'
  | 'add_computed_field'
  | 'drop_computed_field'
  | 'add_source'
  | 'add_existing_table_or_view'
  | 'track_function'
  | 'untrack_function'
  | 'rename_relationship'
  | 'create_object_relationship'
  | 'drop_relationship'
  | 'create_array_relationship'
  | 'create_object_relationship'
  | 'create_array_relationship';

export type MetadataQueries = Record<Driver, Record<MetadataQueryType, string>>;

type MetadataQueryArgs = {
  source: string;
  [key: string]: any;
};

export const getMetadataQuery = (
  type: MetadataQueryType,
  args: MetadataQueryArgs,
  options?: { version: number }
): {
  type: string;
  args: MetadataQueryArgs;
  version?: number;
} => {
  // const prefix = currentDriver === 'postgres' ? 'pg_' : 'mysq_';
  const prefix = '';
  return {
    type: `${prefix}${type}`,
    args,
    ...options,
  };
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

  return getMetadataQuery(queryType, {
    table: tableDef,
    role,
    permission,
    source,
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
  return getMetadataQuery(queryType, {
    table: tableDef,
    role,
    source,
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
  customColumnNames: Record<string, string>
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
  tableDef: QualifiedTable,
  isEnum: boolean,
  source: string
) => {
  return getMetadataQuery('set_table_is_enum', {
    table: tableDef,
    is_enum: isEnum,
    source,
  });
};

export const getTrackTableQuery = (
  tableDef: QualifiedTable,
  source: string
) => {
  return getMetadataQuery('add_existing_table_or_view', {
    table: tableDef,
    source,
  });
};

export const getUntrackTableQuery = (
  tableDef: QualifiedTable,
  source: string
) => {
  return getMetadataQuery('untrack_table', { table: tableDef, source });
};

export const getAddComputedFieldQuery = (
  tableDef: QualifiedTable,
  computedFieldName: string,
  definition: any,
  comment: string,
  source: string
) => {
  return getMetadataQuery('add_computed_field', {
    table: tableDef,
    name: computedFieldName,
    definition,
    comment,
    source,
  });
};

export const getDropComputedFieldQuery = (
  tableDef: QualifiedTable,
  computedFieldName: string,
  source: string
) => {
  return getMetadataQuery('drop_computed_field', {
    table: tableDef,
    name: computedFieldName,
    source,
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

export const generateReplaceMetadataQuery = (
  metadataJson: HasuraMetadataV2
) => ({
  type: 'replace_metadata',
  args: metadataJson,
});

export const resetMetadataQuery = {
  type: 'clear_metadata',
  args: {},
};

export const generateCreateEventTriggerQuery = (
  state: LocalEventTriggerState,
  replace = false
) => ({
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
});

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

export const getDropRemoteRelQuery = (name: string, table: QualifiedTable) => ({
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

export const getBulkQuery = (args: any[]) => {
  return {
    type: 'bulk',
    args,
  };
};

export const addExistingTableOrView = (
  tableName: string,
  schemaName: string,
  source: string
) =>
  getMetadataQuery('add_existing_table_or_view', {
    name: tableName,
    schema: schemaName,
    source,
  });

export const getTrackFunctionQuery = (
  name: string,
  schema: string,
  source: string
) => getMetadataQuery('track_function', { name, schema, source });

export const getTrackFunctionV2Query = (
  name: string,
  schema: string,
  configuration: Record<string, string>,
  source: string
) =>
  getMetadataQuery(
    'track_function',
    {
      function: { name, schema },
      source,
      configuration,
    },
    { version: 2 }
  );

export const getUntrackFunctionQuery = (
  name: string,
  schema: string,
  source: string
) => getMetadataQuery('untrack_function', { name, schema, source });

export const getRenameRelationshipQuery = (
  table: QualifiedTable,
  name: string,
  newName: string,
  source: string
) =>
  getMetadataQuery('rename_relationship', {
    table,
    name,
    new_name: newName,
    source,
  });

export const getCreateObjectRelationshipQuery = (
  table: QualifiedTable,
  name: string,
  source: string
) =>
  getMetadataQuery('create_object_relationship', {
    name,
    table,
    using: {},
    source,
  });

export const getDropRelationshipQuery = (
  table: QualifiedTable,
  name: string,
  source: string
) =>
  getMetadataQuery('drop_relationship', {
    table,
    relationship: name,
    source,
  });

export const getCreateArrayRelationshipQuery = (
  table: QualifiedTable,
  name: string,
  source: string
) =>
  getMetadataQuery('create_array_relationship', {
    name,
    table,
    using: {},
    source,
  });
