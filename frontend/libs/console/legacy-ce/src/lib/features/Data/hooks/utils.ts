import to from 'await-to-js';
import { AxiosError } from 'axios';
import { useMutation } from 'react-query';
import { Feature, RunSQLAPIError, RunSQLResponse } from '../../DataSource';
import { parseRunSQLErrors } from '../errorUtils';
import { hasuraToast } from '../../../new-components/Toasts';
import { Source } from '../../hasura-metadata-types';

/**
 *
 * Pass an instance of react-query useMutation into this function and it returns an additional function `mutateAsyncTuple()`. `mutateAsyncTuple()` is identical to `mutateAsync()` except returns to a tuple like this:
 *
 * const [err, data] = mutateAsyncTuple({ param: 'foo' });
 *
 * The first tuple item `err` is null if no error occurs, so errors can be handled like this:
 *
 * const [err, data] = mutateAsyncTuple({ param: 'foo' });
 *
 * if (err) {
 *  //handle error
 * }
 *
 */
export function addMutateAsyncTuple<
  DataType extends RunSQLResponse | Feature,
  ErrorType extends AxiosError<RunSQLAPIError>,
  ArgsType extends Record<string, any>
>(query: ReturnType<typeof useMutation<DataType, ErrorType, ArgsType>>) {
  return {
    ...query,
    mutateAsyncTuple: (args: ArgsType) =>
      to<DataType, ErrorType>(query.mutateAsync(args)),
  };
}

export const handleRunSqlError = (err: AxiosError<RunSQLAPIError>) => {
  const { title, message } = parseRunSQLErrors(err);
  hasuraToast({
    type: 'error',
    title,
    message,
  });
};

export const getSourceDriver = (sources: Source[], dataSourceName: string) =>
  sources.find(source => source.name === dataSourceName)?.kind;
