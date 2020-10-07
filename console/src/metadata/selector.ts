import { createSelector } from 'reselect';
import { ReduxState } from '../types';
import {
  TableEntry,
  HasuraMetadataV2,
  HasuraMetadataV3,
  DataSource,
} from './types';
import { filterInconsistentMetadataObjects } from '../components/Services/Settings/utils';
import { parseCustomTypes } from '../shared/utils/hasuraCustomTypeUtils';
import { Driver } from '../dataSources';
import { EventTrigger } from '../components/Services/Events/types';

const isMetadataV3 = (
  x: HasuraMetadataV2 | HasuraMetadataV3 | null
): x is HasuraMetadataV3 => {
  return x?.version === 3;
};

export const getDataSourceMetadata = (state: ReduxState) => {
  if (isMetadataV3(state.metadata.metadataObject)) {
    const currentDataSource = state.tables.currentDataSource;
    if (!currentDataSource) return null;
    return state.metadata.metadataObject.sources.find(
      source => source.name === currentDataSource
      // NOTE: Commented this since, kind is not being mentioned on the metadata object atm
      // &&
      // (source.kind || 'postgres') === currentDriver
    );
  }
  return state.metadata.metadataObject;
};

export const getRemoteSchemas = (state: ReduxState) => {
  if (isMetadataV3(state.metadata.metadataObject)) {
    return state.metadata.metadataObject?.remote_schemas ?? [];
  }
  return state.metadata.metadataObject?.remote_schemas ?? [];
};

export const getInitDataSource = (
  state: ReduxState
): { source: string; driver: Driver } => {
  if (isMetadataV3(state.metadata.metadataObject)) {
    const dataSources = state.metadata.metadataObject.sources;
    // .filter(
    //   source => source.name !== 'default'
    // );
    if (dataSources.length) {
      return {
        source: state.metadata.metadataObject.sources[0].name,
        driver: state.metadata.metadataObject.sources[0].kind || 'postgres',
      };
    }
  }
  return { source: '', driver: 'postgres' };
};

const getCurrentSchema = (state: ReduxState) => {
  return state.tables.currentSchema;
};

const getInconsistentObjects = (state: ReduxState) => {
  return state.metadata.inconsistentObjects;
};

const getTables = createSelector(getDataSourceMetadata, source => {
  return source?.tables || [];
});

const getActions = createSelector(
  getDataSourceMetadata,
  source => source?.actions || []
);

const getMetadata = (state: ReduxState) => {
  return state.metadata.metadataObject;
};

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
  [getTables, getActions],
  (tables, actions) => {
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

const getFunctions = createSelector(
  getDataSourceMetadata,
  source =>
    source?.functions?.map(f => ({
      ...f.function,
      function_name: f.function.name,
      function_schema: f.function.schema,
      configuration: f.configuration,
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
  [getDataSourceMetadata, getInconsistentObjects],
  (source, objects) => {
    const actions =
      source?.actions?.map(action => ({
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

export const customTypesSelector = createSelector(
  getDataSourceMetadata,
  source => {
    if (!source) return [];

    return parseCustomTypes(source.custom_types || []);
  }
);

export const getRemoteSchemaSelector = createSelector(
  getRemoteSchemas,
  schemas => (name: string) => {
    return schemas.find(schema => schema.name === name);
  }
);

export const getEventTriggers = createSelector(
  getDataSourceMetadata,
  source => {
    if (!source) return [];

    return source.tables.reduce((acc, t) => {
      const triggers: EventTrigger[] =
        t.event_triggers?.map(trigger => ({
          table_name: t.table.name,
          schema_name: t.table.schema,
          name: trigger.name,
          comment: '',
          configuration: {
            definition: trigger.definition as any, // todo
            headers: trigger.headers || [],
            retry_conf: trigger.retry_conf,
            webhook: trigger.webhook || '',
            webhook_from_env: trigger.webhook_from_env,
          },
        })) || [];
      return [...triggers, ...acc];
    }, [] as EventTrigger[]);
  }
);

export const getAllowedQueries = (state: ReduxState) =>
  state.metadata.allowedQueries || [];

export const getDataSources = createSelector(getMetadata, metadata => {
  if (isMetadataV3(metadata)) {
    console.log({ metadata });
    const sources: DataSource[] = [];
    metadata.sources.forEach(source => {
      sources.push({
        name: source.name,
        url:
          source.configuration?.database_url || 'HASURA_GRAPHQL_DATABASE_URL',
        fromEnv: false, // todo
        connection_pool_settings: source.configuration
          ?.connection_pool_settings || {
          retries: 1,
          idle_timeout: 180,
          max_connections: 50,
        },
        driver: source.kind || 'postgres',
      });
    });
    return sources;
    // .filter(source => source.name !== 'default');
  }

  return [];
});
