import {
  keyToPermission,
  permKeys,
} from '@/components/Services/Data/mergeData';
import type { Permission, ComputedField } from '@/dataSources/types';
import type {
  QualifiedTable,
  RemoteRelationship,
  TableEntry,
} from '@/metadata/types';
import { MetadataResponse } from '..';

export namespace MetadataSelector {
  export const getTablesFromAllSources = (
    m: MetadataResponse
  ): (TableEntry & { source: string })[] => {
    return (
      m.metadata?.sources.reduce((accTables, source) => {
        return accTables.concat(
          source.tables.map(t => ({ ...t, source: source.name }))
        );
      }, [] as (TableEntry & { source: string })[]) || []
    );
  };

  export const getSecuritySettings = (m: MetadataResponse) => {
    const { api_limits, graphql_schema_introspection } = m.metadata ?? {};
    return { api_limits, graphql_schema_introspection };
  };

  export const getDataSourceMetadata = (currentDataSource: string) => (
    m: MetadataResponse
  ) => {
    if (!currentDataSource) return;
    return m.metadata?.sources.find(
      source => source.name === currentDataSource
    );
  };

  export const getTables = (currentDataSource: string) => (
    m: MetadataResponse
  ) => {
    const sources = getDataSourceMetadata(currentDataSource)(m);
    return sources?.tables ?? [];
  };

  export const getTable = (
    currentDataSource: string,
    table: QualifiedTable
  ) => (m: MetadataResponse) => {
    const tables = getTables(currentDataSource)(m);
    return tables.find(
      t => t.table.name === table.name && t.table.schema === table.schema
    );
  };

  export const getTablePermissions = (
    currentDataSource: string,
    table: QualifiedTable
  ) => (m: MetadataResponse) => {
    const metadataTable = getTable(currentDataSource, table)(m);
    const rolePermMap = permKeys.reduce((rpm: Record<string, any>, key) => {
      if (metadataTable) {
        metadataTable[key]?.forEach(
          (perm: { role: string; permission: Record<string, any> }) => {
            rpm[perm.role] = {
              permissions: {
                ...(rpm[perm.role] && rpm[perm.role].permissions),
                [keyToPermission[key]]: perm.permission,
              },
            };
          }
        );
      }
      return rpm;
    }, {});
    const permissions: Permission[] = Object.keys(rolePermMap).map(role => ({
      role_name: role,
      permissions: rolePermMap[role].permissions,
      table_name: table.name,
      table_schema: table.schema,
    }));
    return permissions;
  };

  export const getTableComputedFields = (
    currentDataSource: string,
    table: QualifiedTable
  ) => (m: MetadataResponse) => {
    const metadataTable = getTable(currentDataSource, table)(m);
    const computed_fields: ComputedField[] = (
      metadataTable?.computed_fields || []
    ).map(field => ({
      comment: field.comment || '',
      computed_field_name: field.name,
      name: field.name,
      table_name: table.name,
      table_schema: table.schema,
      definition: field.definition as ComputedField['definition'],
    }));
    return computed_fields;
  };

  export const getRemoteDatabaseRelationships = (
    currentDataSource: string,
    table: QualifiedTable
  ) => (m: MetadataResponse) => {
    const metadataTable = getTable(currentDataSource, table)(m);
    const remote_database_relationships: RemoteRelationship[] = (
      metadataTable?.remote_relationships ?? []
    ).filter(field => 'to_source' in field.definition);
    return remote_database_relationships;
  };

  export const getRemoteSchemaRelationships = (
    currentDataSource: string,
    table: QualifiedTable
  ) => (m: MetadataResponse) => {
    const metadataTable = getTable(currentDataSource, table)(m);
    const remote_schema_relationships: RemoteRelationship[] = (
      metadataTable?.remote_relationships ?? []
    ).filter(field => 'to_remote_schema' in field.definition);
    return remote_schema_relationships;
  };
}
