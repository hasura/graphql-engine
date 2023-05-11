import {
  isLegacyRemoteSchemaRelationship,
  isManualArrayRelationship,
  isManualObjectRelationship,
  isRemoteSchemaRelationship,
} from '../../DataSource';
import { MetadataTable } from '../../hasura-metadata-types';
import {
  LocalRelationship,
  Relationship,
  RemoteDatabaseRelationship,
  RemoteSchemaRelationship,
  SuggestedRelationship,
} from '../types';
import {
  adaptLegacyRemoteSchemaRelationship,
  adaptLocalArrayRelationshipWithFkConstraint,
  adaptLocalArrayRelationshipWithManualConfiguration,
  adaptLocalObjectRelationshipWithFkConstraint,
  adaptLocalObjectRelationshipWithManualConfiguration,
  adaptRemoteDatabaseRelationship,
  adaptRemoteSchemaRelationship,
} from '../utils/adaptResponse';

export const getTableLocalRelationships = (
  metadataTable: MetadataTable | undefined,
  dataSourceName: string,
  suggestedRelationships: SuggestedRelationship[]
) => {
  const table = metadataTable?.table;
  // adapt local array relationships
  const localArrayRelationships = (
    metadataTable?.array_relationships ?? []
  ).map<LocalRelationship>(relationship => {
    if (isManualArrayRelationship(relationship))
      return adaptLocalArrayRelationshipWithManualConfiguration({
        table,
        dataSourceName,
        relationship,
      });

    return adaptLocalArrayRelationshipWithFkConstraint({
      table,
      dataSourceName,
      relationship,
      suggestedRelationships,
    });
  });

  // adapt local object relationships
  const localObjectRelationships = (
    metadataTable?.object_relationships ?? []
  ).map<LocalRelationship>(relationship => {
    if (isManualObjectRelationship(relationship))
      return adaptLocalObjectRelationshipWithManualConfiguration({
        table,
        dataSourceName,
        relationship,
      });
    return adaptLocalObjectRelationshipWithFkConstraint({
      table,
      dataSourceName,
      relationship,
      suggestedRelationships,
    });
  });

  return [...localArrayRelationships, ...localObjectRelationships];
};

export const getAllTableRelationships = (
  metadataTable: MetadataTable | undefined,
  dataSourceName: string,
  suggestedRelationships: SuggestedRelationship[]
): Relationship[] => {
  const table = metadataTable?.table;
  // adapt local array relationships
  const localRelationships = getTableLocalRelationships(
    metadataTable,
    dataSourceName,
    suggestedRelationships
  );

  const remoteRelationships = (metadataTable?.remote_relationships ?? []).map<
    RemoteSchemaRelationship | RemoteDatabaseRelationship
  >(relationship => {
    if (isRemoteSchemaRelationship(relationship))
      return adaptRemoteSchemaRelationship({
        table,
        dataSourceName,
        relationship,
      });

    if (isLegacyRemoteSchemaRelationship(relationship))
      return adaptLegacyRemoteSchemaRelationship({
        table,
        dataSourceName,
        relationship,
      });

    return adaptRemoteDatabaseRelationship({
      table,
      dataSourceName,
      relationship,
    });
  });

  return [...localRelationships, ...remoteRelationships];
};
