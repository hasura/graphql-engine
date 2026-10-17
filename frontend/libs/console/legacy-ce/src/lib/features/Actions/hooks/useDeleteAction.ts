import { useCallback } from 'react';
import { useMetadataMigration } from '@hasura/metadata/api';
import {
  hasuraToast,
  showErrorNotification,
  useDestructivePrompt,
} from '@hasura/shared/ui';
import { removePersistedDerivedAction } from '../utils';

export const generateDropActionQuery = (actionName: string) => ({
  type: 'drop_action' as const,
  args: {
    name: actionName,
  },
});

const useDeleteAction = () => {
  const { mutateAsync, ...mutation } = useMetadataMigration();
  const destructivePrompt = useDestructivePrompt();

  const deleteAction = useCallback(
    // Returns the dialog's promise, which settles when the dialog closes
    // (cancel/mismatch) or once the deletion has completed. Existing callers
    // (e.g. OASGeneratorPage) chain `.finally()` on it for their loading state,
    // so loading stays active while the confirmation is pending.
    async (actionName: string, onSuccess?: () => unknown) =>
      destructivePrompt({
        resourceName: actionName,
        resourceType: 'action',
        destroyTerm: 'delete',
        appendTerm: 'This also deletes its associated metadata.',
        onConfirm: () =>
          mutateAsync({
            query: generateDropActionQuery(actionName),
          })
            .then(() => {
              onSuccess?.();
              removePersistedDerivedAction(actionName);

              hasuraToast({
                title: 'Action deleted successfully!',
                type: 'success',
              });

              return true;
            })
            .catch((error) => {
              showErrorNotification({
                title: 'Deleting action failed!',
                error,
              });

              return false;
            }),
      }),
    [mutateAsync, destructivePrompt],
  );

  return {
    deleteAction,
    ...mutation,
  };
};

export default useDeleteAction;
