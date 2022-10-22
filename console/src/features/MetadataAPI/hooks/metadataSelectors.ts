import {
  keyToPermission,
  permKeys,
} from '@/components/Services/Data/mergeData';
import type { Permission, ComputedField } from '@/dataSources/types';
import { DataTarget } from '@/features/Datasources';

import type {
  QualifiedTable,
  RemoteRelationship,
  rsToRsRelDef,
  TableEntry,
  rsToDbRelDef,
  ObjectRelationship,
  ArrayRelationship,
} from '@/metadata/types';
import { MetadataResponse } from '..';

export namespace MetadataSelector {
  export const getAllSources = () => (m: MetadataResponse) => {
    return m.metadata?.sources;
  };

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

  export const getDataSourceMetadata = (dataSource: string) => (
    m: MetadataResponse
  ) => {
    // TODO: open a issue about return type
    if (!dataSource) return;
    return m.metadata?.sources.find(source => source.name === dataSource);
  };

  export const listRemoteSchemas = () => (m: MetadataResponse) => {
    return m?.metadata?.remote_schemas?.map(({ name }) => name);
  };

  export const getRemoteSchema = (schemaName: string) => (
    m: MetadataResponse
  ) => {
    const schemaMeta = m?.metadata.remote_schemas?.find(
      s => s.name === schemaName
    );
    return schemaMeta;
  };

  export const getAllRemoteSchemaRelationships = () => (
    m: MetadataResponse
  ) => {
    let allRemoteSchemaRelationships: ({ rsName: string } & (
      | rsToDbRelDef
      | rsToRsRelDef
    ))[] = [];

    m.metadata?.remote_schemas?.forEach(rs => {
      const rsName = rs?.name;

      if (rs?.remote_relationships) {
        const temp_rels = rs.remote_relationships.map(rel => {
          return { ...rel, rsName };
        });

        allRemoteSchemaRelationships = [
          ...allRemoteSchemaRelationships,
          ...temp_rels,
        ];
      }
    });
    return allRemoteSchemaRelationships;
  };

  export const getRemoteSchemaRelationship = (sourceRemoteSchema: string) => (
    m: MetadataResponse
  ) => {
    return m?.metadata?.remote_schemas?.find(
      rs => rs.name === sourceRemoteSchema
    );
  };

  export const getDbToRemoteSchemaRelationships = (target: DataTarget) => (
    m: MetadataResponse
  ) => {
    // FIXME: currently the underlying hooks (useTable) only uses QualifiedTable.
    // Qualified table needs to be replaced with DataTarget everywhere so that we can support all targets
    if (!('schema' in target)) {
      throw Error('Big query is not yet supported');
    }

    // find relevant table from metadata
    const tableEntry = m.metadata.sources
      .find(s => s.name === target.database)
      ?.tables.find(
        t => t.table.name === target.table && target.schema === t.table.schema
      );

    // ensure relationship is to remote schema not remote db
    return tableEntry?.remote_relationships?.filter(
      relationship => !relationship.definition.to_source
    );
  };

  export const getTables = (dataSource: string) => (m: MetadataResponse) => {
    const sources = getDataSourceMetadata(dataSource)(m);
    return sources?.tables ?? [];
  };

  export const getTable = (dataSource: string, table: QualifiedTable) => (
    m: MetadataResponse
  ) => {
    const tables = getTables(dataSource)(m);
    return tables.find(
      t => t.table.name === table.name && t.table.schema === table.schema
    );
  };

  export const getTablePermissions = (
    dataSource: string,
    table: QualifiedTable
  ) => (m: MetadataResponse) => {
    const metadataTable = getTable(dataSource, table)(m);
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
    dataSource: string,
    table: QualifiedTable
  ) => (m: MetadataResponse) => {
    const metadataTable = getTable(dataSource, table)(m);
    const computed_fields: ComputedField[] = (
      metadataTable?.computed_fields || []
    ).map(field => ({
      comment: field.comment,
      computed_field_name: field.name,
      name: field.name,
      table_name: table.name,
      table_schema: table.schema,
      definition: field.definition as ComputedField['definition'],
    }));
    return computed_fields;
  };

  export const getRemoteDatabaseRelationships = ({
    target,
  }: {
    target: DataTarget;
  }) => (m: MetadataResponse) => {
    // FIXME: currently the underlying hooks (useTable) only uses QualifiedTable.
    // Qualified table needs to be replaced with DataTarget everywhere so that we can support all targets
    if (!('schema' in target)) {
      throw Error('Big query is not yet supported');
    }

    const metadataTable = getTable(target.database, {
      name: target.table,
      schema: target.schema,
    })(m);
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

  export const getLocalDBObjectRelationships = (
    currentDataSource: string,
    table: QualifiedTable
  ) => (m: MetadataResponse) => {
    const metadataTable = getTable(currentDataSource, table)(m);
    const object_relationships: ObjectRelationship[] =
      metadataTable?.object_relationships ?? [];
    return object_relationships;
  };

  export const getLocalDBArrayRelationships = (
    currentDataSource: string,
    table: QualifiedTable
  ) => (m: MetadataResponse) => {
    const metadataTable = getTable(currentDataSource, table)(m);
    const array_relationships: ArrayRelationship[] =
      metadataTable?.array_relationships ?? [];
    return array_relationships;
  };

  export const createGetLocalDBRelationships = (
    currentDataSource: string,
    table: QualifiedTable
  ) => (m: MetadataResponse) => {
    const metadataTable = getTable(currentDataSource, table)(m);
    const objectRelationships: ObjectRelationship[] =
      metadataTable?.object_relationships ?? [];
    const arrayRelationships: ArrayRelationship[] =
      metadataTable?.array_relationships ?? [];

    return {
      objectRelationships,
      arrayRelationships,
    };
  };

  export const getAllDriversList = (m: MetadataResponse) =>
    m.metadata?.sources.map(s => ({ source: s.name, kind: s.kind }));
}
