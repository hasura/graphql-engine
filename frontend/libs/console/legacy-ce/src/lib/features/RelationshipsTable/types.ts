import {
  ArrayRelationship,
  ObjectRelationship,
  RemoteRelationship,
} from '../../metadata/types';

export type RelationshipType =
  | ObjectRelationship
  | ArrayRelationship
  | RemoteRelationship
  | any; // TODO to_source is not supported on the latest metadata
export type RelationshipSourceType =
  | 'local_object'
  | 'local_array'
  | 'to_source'
  | 'to_remote_schema'
  | 'remote_schema_legacy';
