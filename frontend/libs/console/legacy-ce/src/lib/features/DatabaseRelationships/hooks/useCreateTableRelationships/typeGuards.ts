import {
  AllowedRelationshipDefinitions,
  LocalTableRelationshipDefinition,
  RemoteSchemaRelationshipDefinition,
  RemoteTableRelationshipDefinition,
} from './types';

export const isLocalTableRelationshipDefinition = (
  value: AllowedRelationshipDefinitions
): value is LocalTableRelationshipDefinition => {
  return 'toSource' in value.target;
};

export const isRemoteTableRelationshipDefinition = (
  value: AllowedRelationshipDefinitions
): value is RemoteTableRelationshipDefinition => {
  return 'toRemoteSource' in value.target;
};

export const isRemoteSchemaRelationshipDefinition = (
  value: AllowedRelationshipDefinitions
): value is RemoteSchemaRelationshipDefinition => {
  return 'toRemoteSchema' in value.target;
};
