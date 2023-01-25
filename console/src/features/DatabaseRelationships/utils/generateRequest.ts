import { allowedMetadataTypes } from '@/features/MetadataAPI';
import { LocalRelationship } from '../types';

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
