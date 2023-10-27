import React from 'react';

import { useGetRemoteSchemaRelationship } from '../../../../../MetadataAPI';
import { rsToRsRelDef } from '../../../../../../metadata/types';
import { RsToRsSchema } from '../../../types';

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

  const rsToRsRelationship = data?.remote_relationships?.find(
    ({ type_name: existingTypeName }) => typeName === existingTypeName
  ) as rsToRsRelDef;

  const relationship = rsToRsRelationship?.relationships?.find(
    ({ name }) => name === remoteRelationshipName
  );

  const relationshipInfo = relationship?.definition?.to_remote_schema;

  const defaultValues: RsToRsSchema = React.useMemo(
    () => ({
      relationshipMethod: 'remoteSchema',
      name: relationship?.name || '',
      sourceRemoteSchema: sourceRemoteSchema || '',
      rsSourceType: rsToRsRelationship?.type_name || '',
      referenceRemoteSchema: relationshipInfo?.remote_schema || '',
      resultSet: relationshipInfo?.remote_field || '',
      selectedOperation:
        Object.keys(relationshipInfo?.remote_field ?? {})?.[0] ?? '',
      relationship,
    }),
    [relationship, relationshipInfo, rsToRsRelationship, sourceRemoteSchema]
  );

  return { data: defaultValues, isLoading, isError };
};
