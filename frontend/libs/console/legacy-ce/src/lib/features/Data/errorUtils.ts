const PLACEHOLDER_ERROR_TITLE = 'Error!';
const PLACEHOLDER_ERROR_MESSAGE = 'Something went wrong';

export const parseRunSQLErrors = (err: Record<string, any>) => {
  return {
    title: err?.error ?? PLACEHOLDER_ERROR_TITLE,
    message: err?.internal?.error?.message ?? PLACEHOLDER_ERROR_MESSAGE,
  };
};
