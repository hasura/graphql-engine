import {
  isLegacyRemoteSchemaRelationship,
  isManualArrayRelationship,
  isManualObjectRelationship,
  isRemoteSchemaRelationship,
} from '../../DataSource';
import { MetadataTable } from '../../hasura-metadata-types';
import { SuggestedRelationshipWithName } from '../components/SuggestedRelationships/hooks/useSuggestedRelationships';
import {
  LocalRelationship,
  Relationship,
  RemoteDatabaseRelationship,
  RemoteSchemaRelationship,
  // SuggestedRelationship,
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
  suggestedRelationships: SuggestedRelationshipWithName[]
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

    const arraySuggestedRelationship = suggestedRelationships.filter(
      rel => rel.type === 'array'
    );
    return adaptLocalArrayRelationshipWithFkConstraint({
      table,
      dataSourceName,
      relationship,
      suggestedRelationships: arraySuggestedRelationship,
    });
  });

  const localObjectRelationships = (
    metadataTable?.object_relationships ?? []
  ).map<LocalRelationship>(relationship => {
    if (isManualObjectRelationship(relationship))
      return adaptLocalObjectRelationshipWithManualConfiguration({
        table,
        dataSourceName,
        relationship,
      });

    const objectSuggestedRelationship = suggestedRelationships.filter(
      rel => rel.type === 'object'
    );

    return adaptLocalObjectRelationshipWithFkConstraint({
      table,
      dataSourceName,
      relationship,
      suggestedRelationships: objectSuggestedRelationship,
    });
  });

  return [...localArrayRelationships, ...localObjectRelationships];
};

export const getAllTableRelationships = (
  metadataTable: MetadataTable | undefined,
  dataSourceName: string,
  suggestedRelationships: SuggestedRelationshipWithName[]
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
