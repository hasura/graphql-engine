import { useState } from 'react';
import { getDependencyErrors } from '@hasura/shared/utils';

/**
 * State of a `DestructiveDialogCascade`: whether it is open (and for what,
 * e.g. 'untrack' or 'delete'), and the failure of the last attempt.
 *
 * Failures caused by dependent objects stay in the dialog, which asks the user
 * to enable cascade and try again. Any other failure closes the dialog.
 */
export const useCascadeDialog = <T = true>() => {
  const [openFor, setOpenFor] = useState<{ value: T }>();
  const [error, setError] = useState<unknown>();

  const open = (value: T) => {
    setError(undefined);
    setOpenFor({ value });
  };

  const close = () => {
    setOpenFor(undefined);
    setError(undefined);
  };

  /**
   * Shows the failure in the dialog if it is caused by dependent objects.
   * Otherwise closes the dialog and passes the failure to `onOtherError`, e.g.
   * to show a toast.
   */
  const handleError = (err: unknown, onOtherError: (err: unknown) => void) => {
    if (getDependencyErrors(err)) {
      setError(err);
      return;
    }
    close();
    onOtherError(err);
  };

  return {
    isOpen: openFor !== undefined,
    openFor: openFor?.value,
    open,
    close,
    handleError,
    // clears the failure of the last attempt, e.g. when retrying
    clearError: () => setError(undefined),
    // props for `DestructiveDialogCascade`
    dialogProps: { error, onClose: close },
  };
};
