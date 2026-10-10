import { useQuery } from '@tanstack/react-query';
import { ALL_FRAMEWORKS_FILE_PATH } from '../constants';
import { HttpError } from '@hasura/shared/types';
import { request } from '@hasura/shared/utils';

export type Options<FinalResult> = {
  select?: (m: CodegenFramworkPath[]) => FinalResult;
  onSuccess?: (data: FinalResult) => void;
  staleTime?: number;
  enabled?: boolean;
};

export type CodegenFramworkPath = { name: string; hasStarterKit: boolean };

export const GET_ALL_CODEGEN_FRAMEWORK_PATHS_QUERY_KEY =
  'GET_ALL_CODEGEN_FRAMEWORK_PATHS';

const useGetAllCodegenFrameworks = <FinalResult = CodegenFramworkPath[]>(
  options?: Options<FinalResult>,
) => {
  const queryReturn = useQuery<CodegenFramworkPath[], HttpError, FinalResult>({
    queryKey: [GET_ALL_CODEGEN_FRAMEWORK_PATHS_QUERY_KEY],
    queryFn: async () => {
      const fetchOptions = {
        method: 'GET',
      };

      return request(ALL_FRAMEWORKS_FILE_PATH, fetchOptions).then((response) =>
        response.json(),
      );
    },
    staleTime: Infinity,
    ...options,
  });

  return queryReturn;
};

export default useGetAllCodegenFrameworks;
