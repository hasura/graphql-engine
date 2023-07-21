import { QueryKey, UseQueryOptions, useQuery } from 'react-query';
import { APIError } from '../../hooks/error';

export const DEFAULT_STALE_TIME = 5 * 60000; // 5 minutes as default stale time

export const getDefaultQueryOptions = <
  ReturnType,
  ErrorType = APIError,
  FinalResult = ReturnType,
  TQueryKey extends QueryKey = QueryKey
>(): UseQueryOptions<ReturnType, ErrorType, FinalResult, TQueryKey> => ({
  refetchOnWindowFocus: false,
  staleTime: DEFAULT_STALE_TIME,
});

type useConsoleQueryProps<
  QueryReturnType,
  FinalResult = QueryReturnType,
  ErrorType = APIError,
  TQueryKey extends QueryKey = QueryKey
> = UseQueryOptions<QueryReturnType, ErrorType, FinalResult, TQueryKey>;

// a simple wrapper around useQuery that provides some basic default options for sanity and default error types
export const useConsoleQuery = <
  ReturnType,
  FinalResult = ReturnType,
  ErrorType = APIError,
  TQueryKey extends QueryKey = QueryKey
>(
  options: useConsoleQueryProps<ReturnType, FinalResult, ErrorType, TQueryKey>
) => {
  return useQuery<ReturnType, ErrorType, FinalResult, TQueryKey>({
    ...getDefaultQueryOptions<ReturnType, ErrorType, FinalResult, TQueryKey>(),
    // defaults will be overwritten by what's provided
    ...options,
  });
};
