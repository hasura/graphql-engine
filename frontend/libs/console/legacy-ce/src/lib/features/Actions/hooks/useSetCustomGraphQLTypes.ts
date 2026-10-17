import { useCallback } from 'react';
import { hydrateTypeRelationships } from '../../../shared/utils/hasuraCustomTypeUtils';
import { useMetadataMigration } from '@hasura/metadata/api';
import {
  hasuraToast,
  showErrorNotification,
  useHasuraAlert,
} from '@hasura/shared/ui';
import { CustomTypes } from '@hasura/shared/types';

export const generateSetCustomTypesQuery = (customTypes: CustomTypes) => {
  return {
    type: 'set_custom_types' as const,
    args: customTypes,
  };
};

const useSetCustomGraphQLTypes = () => {
  const { mutateAsync, ...mutation } = useMetadataMigration();
  const { hasuraConfirm } = useHasuraAlert();

  const setCustomGraphQLTypes = useCallback(
    // Resolves `false` on cancel, otherwise with the mutation's outcome once it
    // completes. Callers that chain `.finally()` therefore keep their loading
    // state active until the whole flow is done.
    (
      {
        newTypes,
        existingTypes,
      }: { newTypes: CustomTypes; existingTypes: CustomTypes },
      onSuccess?: () => unknown,
    ) =>
      new Promise<boolean>((resolve) => {
        hasuraConfirm({
          title: 'Set custom types',
          message: 'This could have an effect on the dependent actions.',
          confirmText: 'Continue',
          onClose: ({ confirmed }) => {
            if (!confirmed) {
              resolve(false);
              return;
            }

            const hydratedTypes = hydrateTypeRelationships(
              newTypes,
              existingTypes,
            );
            const query = generateSetCustomTypesQuery(hydratedTypes);

            mutateAsync({
              query,
            })
              .then(() => {
                onSuccess?.();

                hasuraToast({
                  title: 'Successfully set custom types!',
                  type: 'success',
                });

                return true;
              })
              .catch((error) => {
                showErrorNotification({
                  title: 'Setting custom types failed!',
                  error,
                });

                return false;
              })
              .then(resolve);
          },
        });
      }),
    [mutateAsync, hasuraConfirm],
  );

  return {
    setCustomGraphQLTypes,
    ...mutation,
  };
};

export default useSetCustomGraphQLTypes;
