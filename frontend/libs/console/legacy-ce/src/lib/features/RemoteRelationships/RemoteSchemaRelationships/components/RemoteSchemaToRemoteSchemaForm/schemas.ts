import { z } from 'zod';
import { RemoteRelationship } from '@hasura/shared/types';

export const rsToRsFormSchema = z.object({
  relationshipMethod: z.literal('remoteSchema').or(z.literal('remoteDatabase')),
  name: z.string().min(1, { message: 'Name is required' }),
  rsSourceType: z.string().min(1, 'Source type is required'),
  referenceRemoteSchema: z
    .string()
    .min(1, { message: 'Related remote schema is required' }),
  selectedOperation: z.string(),
  resultSet: z.any(),
});

export type RsToRsSchema = z.infer<typeof rsToRsFormSchema>;

export const getDefaultRemoteRelationshipValues = (
  relationship?: RemoteRelationship,
  typeName?: string,
): RsToRsSchema => {
  const result: RsToRsSchema = {
    relationshipMethod: 'remoteSchema',
    name: relationship?.name || '',
    rsSourceType: typeName ?? '',
    referenceRemoteSchema: '',
    resultSet: '',
    selectedOperation: '',
  };

  if (!relationship?.definition) {
    return result;
  }

  if ('to_remote_schema' in relationship.definition) {
    result.referenceRemoteSchema =
      relationship.definition.to_remote_schema.remote_schema;
    result.resultSet = relationship.definition.to_remote_schema.remote_field;
    result.selectedOperation = Object.keys(
      relationship.definition.to_remote_schema.remote_field ?? {},
    )?.[0];
  } else if ('remote_schema' in relationship.definition) {
    result.referenceRemoteSchema = relationship.definition.remote_schema;
    result.resultSet = relationship.definition.remote_field;
    result.selectedOperation = Object.keys(
      relationship.definition.remote_field ?? {},
    )?.[0];
  }

  return result;
};
