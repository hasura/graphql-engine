export const handleDelete = (
  confirmMessage,
  deleteResolver,
  updateDeleteConfirmationError,
  dispatch
) => {
  console.log(
    confirmMessage,
    deleteResolver,
    updateDeleteConfirmationError,
    dispatch
  );

  const a = prompt(confirmMessage);

  try {
    if (a && typeof a === 'string' && a.trim() === 'DELETE') {
      updateDeleteConfirmationError(null);
      dispatch(deleteResolver());
    } else {
      // Input didn't match
      // Show an error message right next to the button
      updateDeleteConfirmationError('user confirmation error!');
    }
  } catch (err) {
    console.error(err);
  }
};
