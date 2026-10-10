import { RemoteRelationship } from '@hasura/shared/types';
import { RemoteRelationshipFieldServer } from './remoteRelationshipsUtils';
import { RelationshipSourceType } from './types';

export const getRemoteFieldPath = (
  remoteField?: Record<string, RemoteRelationshipFieldServer>,
): string[] => {
  let resultArray: string[] = [];
  if (!remoteField) return resultArray;
  resultArray.push(Object.keys(remoteField)[0]);
  if (Object.values(remoteField)?.[0]?.field !== undefined) {
    resultArray = [
      ...resultArray,
      ...getRemoteFieldPath(Object.values(remoteField)?.[0]?.field),
    ];
  }
  return resultArray;
};

export const getRemoteSchemaRelationType = (
  relation: RemoteRelationship,
): [
  name: string,
  sourceType: RelationshipSourceType,
  type: 'Object' | 'Array' | 'Remote Source' | 'Remote Schema',
] => {
  if ('to_source' in relation.definition) {
    return [
      relation.definition.to_source.source,
      'to_source',
      relation.definition.to_source.relationship_type === 'array'
        ? 'Array'
        : 'Object',
    ];
  }

  if ('to_remote_schema' in relation.definition) {
    return [
      relation.definition.to_remote_schema.remote_schema,
      'to_remote_schema',
      'Remote Schema',
    ];
  }

  return [
    relation.definition.remote_schema,
    'to_remote_schema',
    'Remote Schema',
  ];
};
