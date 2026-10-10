import { useCallback } from 'react';
import { useMetadataMigration } from '@hasura/metadata/api';
import { getErrorMessage } from '@hasura/shared/utils';
import { hasuraToast } from '@hasura/shared/ui';
import type { CustomTypes } from '@hasura/shared/types';
import { generateSetCustomTypesQuery } from './useSetCustomGraphQLTypes';
import { removeTypeRelationship } from '../utils';

type RemoveActionRelationshipArgs = {
  existingTypes: CustomTypes;
  relationshipName: string;
  typeName: string;
};

const useRemoveActionRelationship = () => {
  const mutation = useMetadataMigration();

  return useCallback(
    async (
      {
        existingTypes,
        relationshipName,
        typeName,
      }: RemoveActionRelationshipArgs,
      onSuccess?: () => unknown,
    ) => {
      const typesWithoutRel = removeTypeRelationship(
        existingTypes,
        typeName,
        relationshipName,
      );

      return mutation.mutate(
        {
          query: generateSetCustomTypesQuery(typesWithoutRel),
        },
        {
          onSuccess: () => {
            onSuccess?.();
            hasuraToast({
              title: 'Relationship removed successfully!',
              type: 'success',
            });
          },
          onError: (err) => {
            hasuraToast({
              title: `Failed to remove the relationship: "${relationshipName}"`,
              message: getErrorMessage(err),
              type: 'error',
            });
          },
        },
      );
    },
    [mutation],
  );
};

export default useRemoveActionRelationship;
