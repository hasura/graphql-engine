import { useCallback } from 'react';
import { useMetadataMigration } from '@hasura/metadata/api';
import { getErrorMessage, getConfirmation } from '@hasura/shared/utils';
import { hasuraToast } from '@hasura/shared/ui';
import { removePersistedDerivedAction } from '../utils';

export const generateDropActionQuery = (actionName: string) => ({
  type: 'drop_action' as const,
  args: {
    name: actionName,
  },
});

const useDeleteAction = () => {
  const mutation = useMetadataMigration();

  return useCallback(
    async (actionName: string, onSuccess?: () => unknown) => {
      const confirmMessage = `This will permanently delete the action "${actionName}" from this table`;
      const isOk = getConfirmation(confirmMessage, true, actionName);
      if (!isOk) {
        return;
      }

      return mutation.mutate(
        {
          query: generateDropActionQuery(actionName),
        },
        {
          onSuccess: () => {
            onSuccess?.();
            removePersistedDerivedAction(actionName);

            hasuraToast({
              title: 'Action deleted successfully!',
              type: 'success',
            });
          },
          onError: (err) => {
            hasuraToast({
              title: 'Deleting action failed!',
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

export default useDeleteAction;
