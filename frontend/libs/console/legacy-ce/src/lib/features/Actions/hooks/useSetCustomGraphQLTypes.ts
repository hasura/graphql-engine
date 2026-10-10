import { useCallback } from 'react';
import { hydrateTypeRelationships } from '../../../shared/utils/hasuraCustomTypeUtils';
import { useMetadataMigration } from '@hasura/metadata/api';
import { getErrorMessage, getConfirmation } from '@hasura/shared/utils';
import { hasuraToast } from '@hasura/shared/ui';
import { CustomTypes } from '@hasura/shared/types';

export const generateSetCustomTypesQuery = (customTypes: CustomTypes) => {
  return {
    type: 'set_custom_types' as const,
    args: customTypes,
  };
};

const useSetCustomGraphQLTypes = () => {
  const mutation = useMetadataMigration();

  return useCallback(
    async (
      {
        newTypes,
        existingTypes,
      }: { newTypes: CustomTypes; existingTypes: CustomTypes },
      onSuccess?: () => unknown,
    ) => {
      const isOk = getConfirmation(
        'This could have an effect on the dependent actions.',
      );
      if (!isOk) {
        return;
      }

      const hydratedTypes = hydrateTypeRelationships(newTypes, existingTypes);
      const query = generateSetCustomTypesQuery(hydratedTypes);

      return mutation.mutate(
        {
          query,
        },
        {
          onSuccess: () => {
            onSuccess?.();

            hasuraToast({
              title: 'Successfully set custom types!',
              type: 'success',
            });
          },
          onError: (err) => {
            hasuraToast({
              title: 'Setting custom types failed!',
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

export default useSetCustomGraphQLTypes;
