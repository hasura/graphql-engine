import { parseRunSQLErrors } from '../errorUtils';
import { hasuraToast } from '@hasura/shared/ui';

export const handleRunSqlError = (err: Record<string, any>) => {
  const { title, message } = parseRunSQLErrors(err);
  hasuraToast({
    type: 'error',
    title,
    message,
  });
};
