import { UseQueryOptions } from 'react-query';
import { APIError } from '../../hooks/error';

export const DEFAULT_STALE_TIME = 5 * 60000; // 5 minutes as default stale time

export const getDefaultQueryOptions = <
  ReturnType,
  ErrorType = APIError,
  FinalResult = ReturnType
>(): UseQueryOptions<ReturnType, ErrorType, FinalResult> => ({
  refetchOnWindowFocus: false,
  staleTime: DEFAULT_STALE_TIME,
});
