import { allowedMetadataTypes } from '../../MetadataAPI';
import {
  LocalRelationship,
  RemoteDatabaseRelationship,
  RemoteSchemaRelationship,
} from '../types';

export const generateRenameLocalRelationshipRequest = ({
  resource_version,
  driver,
  relationship,
  newName,
}: {
  resource_version: number;
  driver: string;
  relationship: LocalRelationship;
  newName: string;
}) => ({
  resource_version,
  type: `${driver}_rename_relationship` as allowedMetadataTypes,
  args: {
    table: relationship.fromTable,
    source: relationship.fromSource,
    name: relationship.name,
    new_name: newName,
  },
});

export const generateCreateLocalRelationshipWithManualConfigurationRequest = ({
  resource_version,
  driver,
  relationship,
}: {
  resource_version: number;
  driver: string;
  relationship: LocalRelationship;
}) => {
  const type =
    relationship.relationshipType === 'Array'
      ? 'create_array_relationship'
      : 'create_object_relationship';

  return {
    resource_version,
    type: `${driver}_${type}` as allowedMetadataTypes,
    args: {
      table: relationship.fromTable,
      source: relationship.fromSource,
      name: relationship.name,
      using: {
        manual_configuration: {
          remote_table: relationship.definition.toTable,
          column_mapping: relationship.definition.mapping,
        },
      },
    },
  };
};

export const generateDeleteLocalRelationshipRequest = ({
  resource_version,
  driver,
  relationship,
}: {
  resource_version: number;
  driver: string;
  relationship: LocalRelationship;
}) => ({
  resource_version,
  type: `${driver}_drop_relationship` as allowedMetadataTypes,
  args: {
    table: relationship.fromTable,
    source: relationship.fromSource,
    relationship: relationship.name,
  },
});

export const generateRemoteRelationshipCreateRequest = ({
  resource_version,
  driver,
  relationship,
}: {
  resource_version: number;
  driver: string;
  relationship: RemoteSchemaRelationship | RemoteDatabaseRelationship;
}) => {
  const type = 'create_remote_relationship';

  return {
    resource_version,
    type: `${driver}_${type}` as allowedMetadataTypes,
    args: {
      name: relationship.name,
      source: relationship.fromSource,
      table: relationship.fromTable,
      definition: {
        ...(relationship.type === 'remoteSchemaRelationship' && {
          to_remote_schema: {
            remote_schema: relationship.definition.toRemoteSchema,
            lhs_fields: relationship.definition.lhs_fields,
            remote_field: relationship.definition.remote_field,
          },
        }),
        ...(relationship.type === 'remoteDatabaseRelationship' && {
          to_source: {
            relationship_type:
              relationship.relationshipType === 'Array' ? 'array' : 'object',
            source: relationship.definition.toSource,
            table: relationship.definition.toTable,
            field_mapping: relationship.definition.mapping,
          },
        }),
      },
    },
  };
};

export const generateRemoteRelationshipEditRequest = ({
  resource_version,
  driver,
  relationship,
}: {
  resource_version: number;
  driver: string;
  relationship: RemoteSchemaRelationship | RemoteDatabaseRelationship;
}) => {
  const type = 'update_remote_relationship';

  return {
    resource_version,
    type: `${driver}_${type}` as allowedMetadataTypes,
    args: {
      name: relationship.name,
      source: relationship.fromSource,
      table: relationship.fromTable,
      definition: {
        ...(relationship.type === 'remoteSchemaRelationship' && {
          to_remote_schema: {
            remote_schema: relationship.definition.toRemoteSchema,
            lhs_fields: relationship.definition.lhs_fields,
            remote_field: relationship.definition.remote_field,
          },
        }),
        ...(relationship.type === 'remoteDatabaseRelationship' && {
          to_source: {
            relationship_type:
              relationship.relationshipType === 'Array' ? 'array' : 'object',
            source: relationship.definition.toSource,
            table: relationship.definition.toTable,
            field_mapping: relationship.definition.mapping,
          },
        }),
      },
    },
  };
};

export const generateRemoteRelationshipDeleteRequest = ({
  resource_version,
  driver,
  relationship,
}: {
  resource_version: number;
  driver: string;
  relationship: RemoteSchemaRelationship | RemoteDatabaseRelationship;
}) => {
  const type = 'delete_remote_relationship';

  return {
    resource_version,
    type: `${driver}_${type}` as allowedMetadataTypes,
    args: {
      name: relationship.name,
      source: relationship.fromSource,
      table: relationship.fromTable,
    },
  };
};
