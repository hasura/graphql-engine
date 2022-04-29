import {
  QualifiedTable,
  RemoteRelationship,
  ToRemoteSchema,
} from '@/metadata/types';
import { DbToRemoteSchemaRelationship } from '../types';

interface TransformDbToRemoteSchemaArgs {
  table: QualifiedTable;
  remote_relationships: RemoteRelationship[];
}

export namespace MetadataTransformer {
  export const transformDbToRemoteSchema = ({
    table,
    remote_relationships,
  }: TransformDbToRemoteSchemaArgs): DbToRemoteSchemaRelationship[] => {
    return (
      remote_relationships?.map(relationship => {
        const { name, definition } = relationship;

        // if to_remote_schema is not defined, it's in the legacy format
        if (!relationship.definition.to_remote_schema) {
          return {
            table,
            relationshipName: name,
            remoteSchemaName: definition.remote_schema || '',
            lhs_fields: definition.hasura_fields || [],
            remote_field: definition.remote_field || {},
          };
        }

        const {
          remote_schema,
          lhs_fields,
          remote_field,
        } = definition.to_remote_schema as ToRemoteSchema;

        // otherwise it's the new format
        return {
          table,
          relationshipName: name,
          remoteSchemaName: remote_schema || '',
          lhs_fields: lhs_fields || [],
          remote_field: remote_field || {},
        };
      }) || []
    );
  };
}
