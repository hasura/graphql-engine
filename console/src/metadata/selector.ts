/* eslint-disable no-restricted-syntax */
import { createSelector } from 'reselect';
import { FixMe, ReduxState } from '../types';
import { TableEntry, DataSource } from './types';
import { filterInconsistentMetadataObjects } from '../components/Services/Settings/utils';
import { parseCustomTypes } from '../shared/utils/hasuraCustomTypeUtils';
import { Driver, drivers } from '../dataSources';
import {
  EventTrigger,
  ScheduledTrigger,
} from '../components/Services/Events/types';

export const getDataSourceMetadata = (state: ReduxState) => {
  const currentDataSource = state.tables?.currentDataSource;
  if (!currentDataSource) return null;
  return state.metadata.metadataObject?.sources.find(
    source => source.name === currentDataSource
  );
};

export const getRemoteSchemas = (state: ReduxState) => {
  return state.metadata.metadataObject?.remote_schemas ?? [];
};
export const getRemoteSchemaPermissions = (state: ReduxState) => {
  return state.remoteSchemas.permissions ?? {};
};

export const getInitDataSource = (
  state: ReduxState
): { source: string; driver: Driver } => {
  const dataSources =
    state.metadata.metadataObject?.sources.filter(s =>
      s.kind ? drivers.includes(s.kind) : true
    ) || [];
  if (dataSources.length) {
    return {
      source: dataSources[0].name,
      driver: dataSources[0].kind || 'postgres',
    };
  }
  return { source: '', driver: 'postgres' };
};

const getCurrentSchema = (state: ReduxState) => {
  return state.tables.currentSchema;
};

const getCurrentTable = (state: ReduxState) => {
  return state.tables.currentTable;
};

export const getCurrentSource = (state: ReduxState) => {
  return state.tables.currentDataSource;
};

const getInconsistentObjects = (state: ReduxState) => {
  return state.metadata.inconsistentObjects;
};
const getSecuritySettings = (state: ReduxState) => {
  const { api_limits, graphql_schema_introspection } =
    state.metadata.metadataObject ?? {};
  return { api_limits, graphql_schema_introspection };
};

export const getTables = createSelector(getDataSourceMetadata, source => {
  return source?.tables || [];
});

export const getTablesFromAllSources = (
  state: ReduxState
): (TableEntry & { source: string })[] => {
  return (
    state.metadata.metadataObject?.sources.reduce((accTables, source) => {
      return accTables.concat(
        source.tables.map(t => ({ ...t, source: source.name }))
      );
    }, [] as (TableEntry & { source: string })[]) || []
  );
};

const getMetadata = (state: ReduxState) => {
  return state.metadata.metadataObject;
};

const getActions = createSelector(
  getMetadata,
  metadata => metadata?.actions || []
);

type PermKeys = Pick<
  TableEntry,
  | 'update_permissions'
  | 'select_permissions'
  | 'delete_permissions'
  | 'insert_permissions'
>;
const permKeys: Array<keyof PermKeys> = [
  'insert_permissions',
  'update_permissions',
  'select_permissions',
  'delete_permissions',
];
export const rolesSelector = createSelector(
  [getTablesFromAllSources, getActions, getRemoteSchemas, getSecuritySettings],
  (tables, actions, remoteSchemas, securitySettings) => {
    const roleNames: string[] = [];
    tables?.forEach(table =>
      permKeys.forEach(key =>
        table[key]?.forEach(({ role }: { role: string }) =>
          roleNames.push(role)
        )
      )
    );
    actions?.forEach(action =>
      action.permissions?.forEach(p => roleNames.push(p.role))
    );
    remoteSchemas?.forEach(remoteSchema => {
      remoteSchema?.permissions?.forEach(p => roleNames.push(p.role));
    });

    Object.entries(securitySettings.api_limits ?? {}).forEach(
      ([limit, value]) => {
        if (limit !== 'disabled' && typeof value !== 'boolean') {
          Object.keys(value?.per_role ?? {}).forEach(role =>
            roleNames.push(role)
          );
        }
      }
    );
    securitySettings.graphql_schema_introspection?.disabled_for_roles.forEach(
      role => roleNames.push(role)
    );
    return Array.from(new Set(roleNames));
  }
);

export const getRemoteSchemasSelector = createSelector(
  [getRemoteSchemas, getInconsistentObjects],
  (schemas, inconsistentObjects) => {
    return filterInconsistentMetadataObjects(
      schemas,
      inconsistentObjects,
      'remote_schemas'
    );
  }
);

export const remoteSchemasNamesSelector = createSelector(
  getRemoteSchemas,
  schemas => schemas?.map(schema => schema.name)
);

type Options = {
  schemas?: string[];
  tables?: {
    table_schema: string;
    table_name: string;
  }[];
};
export const getTablesInfoSelector = createSelector(
  getTables,
  tables => (options: Options) => {
    if (options.schemas) {
      return tables?.filter(t => options?.schemas?.includes(t.table.schema));
    }
    if (options.tables) {
      return tables?.filter(t =>
        options.tables?.find(
          optTable =>
            optTable.table_name === t.table.name &&
            optTable.table_schema === t.table.schema
        )
      );
    }
    return tables;
  }
);

export const getTableInformation = createSelector(
  getTables,
  tables => (tableName: string, tableSchema: string) => <
    T extends keyof TableEntry
  >(
    property: T
  ): TableEntry[T] | null => {
    const table = tables?.find(
      t => tableName === t.table.name && tableSchema === t.table.schema
    );
    return table ? table[property] : null;
  }
);

export const getCurrentTableInformation = createSelector(
  [getTableInformation, getCurrentTable, getCurrentSchema],
  (getTableInfo, tableName, schema) => getTableInfo(tableName, schema)
);

export const getFunctions = createSelector(
  getDataSourceMetadata,
  source =>
    source?.functions?.map(f => ({
      ...f.function,
      function_name: f.function.name,
      function_schema: f.function.schema,
      configuration: f.configuration,
      permissions: f?.permissions,
    })) || []
);

export const getFunctionSelector = createSelector(
  getFunctions,
  functions => (name: string, schema: string) => {
    return functions?.find(
      f => f.function_name === name && f.function_schema === schema
    );
  }
);

export const getConsistentFunctions = createSelector(
  [getFunctions, getInconsistentObjects, getCurrentSchema],
  (funcs, objects, schema) => {
    return filterInconsistentMetadataObjects(
      funcs.filter(f => f.function_schema === schema),
      objects,
      'functions'
    );
  }
);

const getCurrentFunctionInfo = (state: ReduxState) => ({
  name: state.functions.functionName,
  schema: state.functions.functionSchema,
});

export const getFunctionConfiguration = createSelector(
  getFunctions,
  getCurrentFunctionInfo,
  (funcs, { name, schema }) => {
    const func = funcs.find(
      f => f.function_name === name && f.function_schema === schema
    );
    return func?.configuration;
  }
);

export const actionsSelector = createSelector(
  [getMetadata, getInconsistentObjects],
  (metadata, objects) => {
    const actions =
      metadata?.actions?.map(action => ({
        ...action,
        definition: {
          ...action.definition,
          headers: action.definition.headers || [],
        },
        permissions: action.permissions || [],
      })) || [];

    return filterInconsistentMetadataObjects(actions, objects, 'actions');
  }
);

export const customTypesSelector = createSelector(getMetadata, metadata => {
  if (!metadata?.custom_types) return [];

  return parseCustomTypes(metadata.custom_types || []);
});

export const getRemoteSchemaSelector = createSelector(
  getRemoteSchemas,
  schemas => (name: string) => {
    return schemas.find(schema => schema.name === name);
  }
);

export const getEventTriggers = createSelector(getMetadata, metadata => {
  let triggersMap: EventTrigger[] = [];
  if (!metadata) {
    return triggersMap;
  }

  metadata.sources.forEach(source => {
    triggersMap = triggersMap.concat(
      source.tables.reduce((acc, t) => {
        const triggers: EventTrigger[] =
          t.event_triggers?.map(trigger => ({
            table_name: t.table.name,
            schema_name: t.table.schema,
            source: source.name,
            name: trigger.name,
            comment: '',
            configuration: {
              definition: trigger.definition as FixMe,
              headers: trigger.headers || [],
              retry_conf: trigger.retry_conf,
              webhook: trigger.webhook || '',
              webhook_from_env: trigger.webhook_from_env,
            },
          })) || [];
        return [...triggers, ...acc];
      }, [] as EventTrigger[])
    );
  });
  return triggersMap;
});

export const getManualEventsTriggers = createSelector(
  getEventTriggers,
  getCurrentSchema,
  getCurrentTable,
  (triggers, schema, table) => {
    return triggers.filter(
      t =>
        t.table_name === table &&
        t.schema_name === schema &&
        t.configuration.definition.enable_manual
    );
  }
);

export const getEventTriggerByName = createSelector(
  getEventTriggers,
  getMetadata,
  (triggers, metadata) => (name: string) => {
    const currentTrigger = triggers.find(tr => tr.name === name);
    if (currentTrigger) return currentTrigger;

    for (const source of metadata?.sources || []) {
      for (const table of source.tables) {
        const trigger = table.event_triggers?.find(tr => tr.name === name);
        if (trigger)
          return {
            table_name: table.table.name,
            schema_name: table.table.schema,
            source: source.name,
            name: trigger.name,
            comment: '',
            configuration: {
              definition: trigger.definition as FixMe,
              headers: trigger.headers || [],
              retry_conf: trigger.retry_conf,
              webhook: trigger.webhook || '',
              webhook_from_env: trigger.webhook_from_env,
            },
          };
      }
    }

    return null;
  }
);

export const getCronTriggers = createSelector(getMetadata, metadata => {
  const cronTriggers: ScheduledTrigger[] = (metadata?.cron_triggers || []).map(
    cron => ({
      name: cron.name,
      payload: cron.payload,
      retry_conf: {
        num_retries: cron.retry_conf?.num_retries,
        retry_interval_seconds: cron.retry_conf?.retry_interval_seconds,
        timeout_seconds: cron.retry_conf?.timeout_seconds,
        tolerance_seconds: cron.retry_conf?.tolerance_seconds,
      },
      header_conf: cron.headers,
      webhook_conf: cron.webhook,
      cron_schedule: cron.schedule,
      include_in_metadata: cron.include_in_metadata,
      comment: cron.comment,
    })
  );
  return cronTriggers || [];
});

export const getAllowedQueries = (state: ReduxState) =>
  state.metadata.allowedQueries || [];

export const getInheritedRoles = (state: ReduxState) =>
  state.metadata.inheritedRoles || [];

export const getSourcesFromMetadata = createSelector(getMetadata, metadata => {
  return metadata?.sources.filter(s =>
    s.kind ? drivers.includes(s.kind) : true
  );
});

export const getDataSources = createSelector(getMetadata, metadata => {
  const sources: DataSource[] = [];
  metadata?.sources
    .filter(s => (s.kind ? drivers.includes(s.kind) : true))
    .forEach(source => {
      let url: string | { from_env: string } = '';
      if (source.kind === 'bigquery') {
        url = source.configuration?.service_account?.from_env || '';
      } else {
        url = source.configuration?.connection_info?.connection_string
          ? source.configuration?.connection_info.connection_string
          : source.configuration?.connection_info?.database_url || '';
      }
      sources.push({
        name: source.name,
        url,
        connection_pool_settings: source.configuration?.connection_info
          ?.pool_settings || {
          retries: 1,
          idle_timeout: 180,
          max_connections: 50,
        },
        driver: source.kind || 'postgres',
        read_replicas: source.configuration?.read_replicas ?? undefined,
      });
    });
  return sources;
});

export const getTablesBySource = createSelector(getMetadata, metadata => {
  const res: Record<string, { name: string; schema: string }[]> = {};
  metadata?.sources.forEach(source => {
    res[source.name] = source.tables.map(({ table }) => table);
  });
  return res;
});
