import { RemoteRelationship } from '@hasura/shared/types';
import { z } from 'zod';

export const schema = z.object({
  relationshipType: z.literal('array').or(z.literal('object')),
  relationshipName: z.string().min(1, { message: 'Name is required!' }),
  target: z.object({
    type: z.literal('table'),
    dataSourceName: z.string().min(1, 'Reference source is a required field'),
    table: z.any(),
  }),
  mapping: z.array(
    z.object({
      field: z.string(),
      column: z.string(),
    }),
  ),
  typeName: z.string().min(1, { message: 'Type is required!' }),
});

export type Schema = z.infer<typeof schema>;

export const getDefaultRemoteSchemaToDbValues = (
  relationship: RemoteRelationship | undefined,
  typeName?: string,
) => {
  const relationshipInfo =
    relationship?.definition && 'to_source' in relationship?.definition
      ? relationship.definition.to_source
      : undefined;

  const defaultValues: Schema = {
    relationshipName: relationship?.name || '',
    target: {
      dataSourceName: relationshipInfo?.source || '',
      table: relationshipInfo?.table,
      type: 'table',
    },
    mapping: relationshipInfo?.field_mapping
      ? Object.entries(relationshipInfo?.field_mapping).map(
          ([field, column]) => ({ field, column }),
        )
      : [],
    typeName: typeName || '',
    relationshipType: relationshipInfo?.relationship_type || 'array',
  };

  return defaultValues;
};
