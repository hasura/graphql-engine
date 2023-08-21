import {
  LogicalModelWithSource,
  NativeQueryWithSource,
} from '../Data/LogicalModels/types';
import { getForeignKeyRelationships as selectForeignKeyRels } from '../Data/TrackResources/TrackRelationships/selectors/selectors';
import { Metadata, QualifiedFunction, Table } from '../hasura-metadata-types';
import * as utils from './utils';

/*

How do I implement my custom selector apart from the ones provided here?

In your local component, write the selector as per your requirement ->

// use the useMetadata from lib
import {useMetadata} from '@/features/hasura-metadata-api`;

// This will be your selector
const getRemoteSchema = (remoteSchemaName: string) => (m: Metadata) => m.metadata.remote_schemas?.find(s => s.name === remoteSchemaName)

// usage
const { isLoading, data: remote_schemas } = useMetadata(getRemoteSchema('foobar'));

*/

export const getSources = () => (m: Metadata) => m?.metadata.sources;

export const findSource = (dataSourceName: string) => (m: Metadata) =>
  utils.findMetadataSource(dataSourceName, m);

export const findNativeQuery =
  (dataSourceName: string, nativeQueryName: string) => (m: Metadata) =>
    utils
      .findMetadataSource(dataSourceName, m)
      ?.native_queries?.find(nq => nq.root_field_name === nativeQueryName);

export const findLogicalModel =
  (dataSourceName: string, logicalModelName: string) => (m: Metadata) =>
    utils
      .findMetadataSource(dataSourceName, m)
      ?.logical_models?.find(lm => lm.name === logicalModelName);

export const getTables = (dataSourceName: string) => (m: Metadata) =>
  utils.findMetadataSource(dataSourceName, m)?.tables;

export const findTable =
  (dataSourceName: string, table: Table) => (m: Metadata) =>
    utils.findMetadataTable(dataSourceName, table, m);

export const findFunction =
  (dataSourceName: string, qualifiedFunction: QualifiedFunction) =>
  (m: Metadata) =>
    utils.findMetadataFunction(dataSourceName, qualifiedFunction, m);

export const resourceVersion = () => (m: Metadata) => m?.resource_version;

export const extractModelsAndQueriesFromMetadata = (
  m: Metadata
): { queries: NativeQueryWithSource[]; models: LogicalModelWithSource[] } => {
  const sources = m.metadata.sources;
  let models: LogicalModelWithSource[] = [];
  let queries: NativeQueryWithSource[] = [];

  sources.forEach(s => {
    if (s.logical_models && s.logical_models.length > 0) {
      models = [...models, ...s.logical_models.map(m => ({ ...m, source: s }))];
    }

    if (s.native_queries && s.native_queries.length > 0) {
      queries = [
        ...queries,
        ...s.native_queries.map(q => ({ ...q, source: s })),
      ];
    }
  });

  return {
    models,
    queries,
  };
};

export const getForeignKeyRelationships =
  (dataSourceName: string) => (m: Metadata) => {
    const source = utils.findMetadataSource(dataSourceName, m);
    const tables = source?.tables ?? [];
    const foreignKeyRelationships = tables
      .map(t => selectForeignKeyRels(t))
      .flat();
    return foreignKeyRelationships;
  };

export const getRoles = (m: Metadata) => {
  const roles: string[] = [];

  /**
   * Actions Permissions
   */
  m.metadata.actions?.forEach(action =>
    action.permissions?.forEach(p => roles.push(p.role))
  );

  /**
   * Table Permissions
   */
  m.metadata.sources.forEach(source => {
    source.tables.forEach(table => {
      table.select_permissions?.forEach(permission =>
        roles.push(permission.role)
      );

      table.insert_permissions?.forEach(permission =>
        roles.push(permission.role)
      );

      table.update_permissions?.forEach(permission =>
        roles.push(permission.role)
      );

      table.delete_permissions?.forEach(permission =>
        roles.push(permission.role)
      );
    });
  });

  /**
   * Remote Schema Permissions
   */
  m.metadata.remote_schemas?.forEach(remoteSchema => {
    remoteSchema?.permissions?.forEach(p => roles.push(p.role));
  });

  /**
   * Allow List
   */
  m.metadata.allowlist?.forEach(al => {
    if (al?.scope?.global === false) {
      al?.scope?.roles?.forEach(role => roles.push(role));
    }
  });

  /**
   * API limits
   */
  Object.entries(m.metadata.api_limits ?? {}).forEach(([limit, value]) => {
    if (limit !== 'disabled' && typeof value !== 'boolean') {
      Object.keys(value?.per_role ?? {}).forEach(role => roles.push(role));
    }
  });

  /**
   * GraphQL introspection limits
   */
  m.metadata?.graphql_schema_introspection?.disabled_for_roles.forEach(role =>
    roles.push(role)
  );

  /**
   * Logical Model Permissions
   */
  m.metadata.sources.forEach(source => {
    source.logical_models?.forEach(logicalModel => {
      logicalModel.select_permissions?.forEach(permission =>
        roles.push(permission.role)
      );
    });
  });

  return Array.from(new Set(roles));
};
