import { AxiosError } from 'axios';
import { RunSQLAPIError } from '../DataSource/api';

const PLACEHOLDER_ERROR_TITLE = 'Error!';
const PLACEHOLDER_ERROR_MESSAGE = 'Something went wrong';

export const parseRunSQLErrors = (err: AxiosError<RunSQLAPIError>) => {
  return {
    title: err.response?.data?.error ?? PLACEHOLDER_ERROR_TITLE,
    message:
      err.response?.data?.internal?.error?.message ?? PLACEHOLDER_ERROR_MESSAGE,
  };
};

export const transformErrorResponse = (error: unknown) => {
  const err = error as Record<string, any>;

  let message = '';

  if ('internal' in err) message = JSON.stringify(err?.internal, null, '\t');
  else message = err.error;

  return {
    name: `Error code: ${err.code}`,
    message,
  };
};
