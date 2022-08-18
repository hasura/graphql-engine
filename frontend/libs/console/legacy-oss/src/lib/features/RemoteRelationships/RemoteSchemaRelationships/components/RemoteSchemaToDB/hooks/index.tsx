import React from 'react';

import { useGetRemoteSchemaRelationship } from '@/features/MetadataAPI';
import { rsToDbRelDef } from '@/metadata/types';

import { Schema } from '../schema';

interface UseDefaultValuesProps {
  sourceRemoteSchema: string;
  remoteRelationshipName?: string;
  typeName?: string;
}

export const useDefaultValues = ({
  sourceRemoteSchema,
  typeName = '',
  remoteRelationshipName = '',
}: UseDefaultValuesProps) => {
  const { data, isLoading, isError } =
    useGetRemoteSchemaRelationship(sourceRemoteSchema);

  const rsToDbRelationship = data?.remote_relationships?.find(
    ({ type_name: existingTypeName }) => typeName === existingTypeName
  ) as rsToDbRelDef;

  const relationship = rsToDbRelationship?.relationships?.find(
    ({ name }) => name === remoteRelationshipName
  );

  const relationshipInfo = relationship?.definition?.to_source;

  const defaultValues: Schema = React.useMemo(
    () => ({
      relationshipName: relationship?.name || '',
      database: relationshipInfo?.source || '',
      schema:
        relationshipInfo?.table.schema ??
        (relationshipInfo?.table as any)?.dataset ??
        '',
      table: relationshipInfo?.table.name || '',
      mapping: Object.entries(relationshipInfo?.field_mapping ?? {}).map(
        ([field, column]) => ({ field, column: column as string })
      ),
      typeName: rsToDbRelationship?.type_name || '',
      sourceRemoteSchema: sourceRemoteSchema || '',
      relationshipType: relationshipInfo?.relationship_type || 'array',
      driver: '',
    }),
    [relationship, relationshipInfo, rsToDbRelationship, sourceRemoteSchema]
  );

  return { data: defaultValues, isLoading, isError };
};
