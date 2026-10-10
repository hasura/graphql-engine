import { useMutation, UseMutationOptions } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { toastMetadataOutOfDateError } from '../../../utils/error';
import { useAppContext } from '@hasura/shared/context';
import { runMetadataQuery, TMigrationQuery } from '../../../api';
import { usePostMetadataMigration } from './usePostMetadataMigration';

export type MetadataMigrationMutationOptions<
  // So, I want some validation on this change from others. The type being used as the return from the metadata query was "RunSQLResponse".
  // This type was not correct for dc_add_agent, and the closer I looked the return seems variable based on the query type.
  // I checked every reference to useMetadataMigration and it did not appear there was anywhere in the code using the onSuccess arguments, so this change seems safe.
  // So, I thought the most flexible way to approach this would be to change the base type to Record<string, any> and allow a dev to pass more specific types if needed.
  // I also added and ArgsType so that the args will be typed if the dev would prefer.
  // after review I can modify this comment and remove the explanation of change.
  ResponseType extends Record<string, any> = Record<string, any>,
  TVariables = unknown,
> = Omit<UseMutationOptions<ResponseType, Error, TVariables>, 'mutationFn'>;

export function useMetadataMigrationMutation<
  ResponseType extends Record<string, any> = Record<string, any>,
  TVariables = unknown,
>(
  getBody: (variables: TVariables) => Promise<TMigrationQuery>,
  mutationOptions?: MetadataMigrationMutationOptions<ResponseType, TVariables>,
) {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const postMetadata = usePostMetadataMigration();

  return useMutation({
    ...mutationOptions,
    mutationFn: async (variables) => {
      const body = await getBody(variables);

      const result = await runMetadataQuery<ResponseType>({
        url: endpoints.metadata,
        fetchJson,
        body,
      });

      postMetadata(variables);

      return result;
    },
    onError: (error, variables, onMutateResult, context) => {
      toastMetadataOutOfDateError(error, () => postMetadata(variables));

      if (mutationOptions?.onError) {
        mutationOptions.onError(error, variables, onMutateResult, context);
      }
    },
  });
}
