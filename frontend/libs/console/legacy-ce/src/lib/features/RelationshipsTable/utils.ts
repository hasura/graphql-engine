import { RemoteRelationshipFieldServer } from '../../components/Services/Data/TableRelationships/RemoteRelationships/utils';
import { RelationshipSourceType, RelationshipType } from './types';

export const getRemoteRelationType = (
  relation: RelationshipType
): [
  name: string,
  sourceType: RelationshipSourceType,
  type: 'Object' | 'Array' | 'Remote Source' | 'Remote Schema'
] => {
  if (relation?.definition?.to_source) {
    return [
      relation?.name,
      'to_source',
      relation?.definition?.to_source?.relationship_type,
    ];
  }
  if (relation?.definition?.to_remote_schema)
    return [
      relation?.source_name ?? relation?.remote_schema,
      'to_remote_schema',
      'Remote Schema',
    ];
  return [relation?.source_name, 'remote_schema_legacy', 'Remote Schema'];
};

export const getRemoteFieldPath = (
  remoteField?: Record<string, RemoteRelationshipFieldServer>
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
  relation: RelationshipType
): [
  name: string,
  sourceType: RelationshipSourceType,
  type: 'Object' | 'Array' | 'Remote Source' | 'Remote Schema'
] => {
  if (relation?.definition?.to_source) {
    return [
      relation?.definition?.to_source.source,
      'to_source',
      relation?.definition?.to_source?.relationship_type,
    ];
  }

  return [
    relation?.definition?.to_remote_schema.remote_schema,
    'to_remote_schema',
    'Remote Schema',
  ];
};
