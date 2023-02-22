import { DataTarget } from '../../Datasources';
import {
  ArrayRelationship,
  DeletePermissionEntry,
  InsertPermissionEntry,
  ObjectRelationship,
  RemoteRelationship,
  SelectPermissionEntry,
  TableEntry,
  ToRemoteSchema,
  UpdatePermissionEntry,
} from '../../../metadata/types';
import {
  TableRelationship,
  DbToDbRelationship,
  DbToRemoteSchemaRelationship,
} from '../types';

export interface TransformDbToRemoteSchemaArgs {
  target: DataTarget;
  remote_relationships: RemoteRelationship[];
}

export interface Permission {
  table: string;
  role: string;
  queryType: string;
}

type PermissionEntry =
  | InsertPermissionEntry
  | UpdatePermissionEntry
  | SelectPermissionEntry
  | DeletePermissionEntry;
export namespace MetadataTransformer {
  // take object and array relationships input
  export const transformTableRelationships = ({
    target,
    relationships,
    tableRelationships,
  }: {
    target: DataTarget;
    relationships: {
      objectRelationships: ObjectRelationship[];
      arrayRelationships: ArrayRelationship[];
    };
    tableRelationships?: {
      from: {
        table: string;
        column: string[];
      };
      to: {
        table: string;
        column: string[];
      };
    }[];
  }) => {
    const objs = relationships.objectRelationships.map(({ name, comment }) => {
      const tableRelationship = tableRelationships?.find(
        ({ from }) => from.table === target.table
      );

      return {
        name,
        comment,
        type: 'object',
        ...tableRelationship,
      };
    });

    const arrs = relationships.arrayRelationships.map(({ name, comment }) => {
      const tableRelationship = tableRelationships?.find(
        ({ to }) => to.table.replace(/['"]+/g, '') === target.table
      );

      return {
        name,
        comment,
        type: 'array',
        ...tableRelationship,
      };
    });

    return [...objs, ...arrs] as TableRelationship[];
  };

  export const transformDbToRemoteSchema = ({
    target,
    remote_relationships,
  }: TransformDbToRemoteSchemaArgs): DbToRemoteSchemaRelationship[] => {
    return (
      remote_relationships?.map(relationship => {
        const { name, definition } = relationship;

        // if to_remote_schema is not defined, it's in the legacy format
        if (!relationship.definition.to_remote_schema) {
          return {
            target,
            relationshipName: name,
            remoteSchemaName: definition.remote_schema || '',
            lhs_fields: definition.hasura_fields || [],
            remote_field: definition.remote_field || {},
          };
        }

        const { remote_schema, lhs_fields, remote_field } =
          definition.to_remote_schema as ToRemoteSchema;

        // otherwise it's the new format
        return {
          target,
          relationshipName: name,
          remoteSchemaName: remote_schema || '',
          lhs_fields: lhs_fields || [],
          remote_field: remote_field || {},
        };
      }) || []
    );
  };
  export const transformDbToDb = ({
    target,
    remote_relationships,
  }: TransformDbToRemoteSchemaArgs): DbToDbRelationship[] => {
    return remote_relationships.map(relationship => {
      const { name, definition } = relationship;

      return {
        target,
        relationshipName: name,
        remoteDbName: definition.to_source.source || '',
        relationshipType: (definition.to_source.relationship_type ||
          'object') as 'object' | 'array',
        fieldMapping: definition.to_source.field_mapping || {},
      };
    });
  };

  export const transformPermissions = (tables: TableEntry[]) => {
    const permissions = tables.reduce((acc, tableEntry) => {
      const containsPermissions = Object.keys(tableEntry).filter(key =>
        key.match(/permission/)
      );
      if (!containsPermissions) {
        return acc;
      }

      Object.entries(tableEntry).forEach(
        ([key, value]: [string, PermissionEntry[]]) => {
          if (key.match(/permission/)) {
            value.forEach(({ role }) => {
              acc.push({
                table: tableEntry.table.name,
                role,
                // get the query type from the metadata
                queryType: key.split('_')[0],
              });
            });
          }
        }
      );

      return acc;
    }, [] as Permission[]);

    return permissions;
  };
}
