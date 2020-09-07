import { createSelector } from 'reselect';
import { ReduxState } from '../types';
import { TableEntry, HasuraMetadataV2 } from './types';
import { filterInconsistentMetadataObjects } from '../components/Services/Settings/utils';
import { parseCustomTypes } from '../shared/utils/hasuraCustomTypeUtils';

const getCurrentSchema = (state: ReduxState) => {
  return state.tables.currentSchema;
};

const getInconsistentObjects = (state: ReduxState) => {
  return state.metadata.inconsistentObjects;
};

const getTables = (state: ReduxState) => {
  return state.metadata.metadataObject?.tables;
};

const getActions = (state: ReduxState) => {
  return state.metadata.metadataObject?.actions;
};

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

const getRemoteSchemas = (state: ReduxState) => {
  return state.metadata.metadataObject?.remote_schemas || [];
};

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

const getFunctions = (state: ReduxState) => {
  return (
    state.metadata.metadataObject?.functions?.map(f => ({
      ...f.function,
      function_name: f.function.name,
      function_schema: f.function.schema,
      configuration: f.configuration,
    })) || []
  );
};

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

const metadataSelector = (state: ReduxState): HasuraMetadataV2 | null => {
  return state.metadata.metadataObject;
};

export const actionsSelector = createSelector(
  [metadataSelector, getInconsistentObjects],
  (metadata, objects) => {
    if (!metadata) return [];

    const actions =
      metadata.actions?.map(action => ({
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
  metadataSelector,
  metadata => {
    if (!metadata) return [];

    return parseCustomTypes(metadata.custom_types || []);
  }
);

export const getRemoteSchemaSelector = createSelector(
  getRemoteSchemas,
  schemas => (name: string) => {
    return schemas.find(schema => schema.name === name);
  }
);

export const getAllowedQueries = (state: ReduxState) =>
  state.metadata.allowedQueries || [];

export const getDataSources = createSelector(getMetadata, metadata => {
  return [
    { name: 'Warehouse DB', driver: 'postgres' },
    { name: 'Users DB', driver: 'mysql' },
  ]; // todo
});
