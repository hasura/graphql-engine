import {
  LocalRelationship,
  Relationship,
  RemoteDatabaseRelationship,
} from '../types';

export function isNotRemoteSchemaRelationship(
  relationship: Relationship,
): relationship is LocalRelationship | RemoteDatabaseRelationship {
  return relationship.type !== 'remoteSchemaRelationship';
}
