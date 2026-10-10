import {
  useMutation,
  UseMutationOptions,
  useQueryClient,
} from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource } from '@hasura/shared/types';
import { hasuraToast } from '@hasura/shared/ui';
import {
  getDatabaseMethods,
  ModifyForeignKeyArgs,
  NotImplementedError,
} from '../../driver';
import { invalidateMetadata } from '@hasura/metadata/api';

type AlterForeignKeyProps = ModifyForeignKeyArgs & {
  source: QualifiedDataSource;
};

type UseAlterForeignKeyPropsOptions = Omit<
  UseMutationOptions<boolean, unknown, AlterForeignKeyProps>,
  'mutationFn'
>;

export function useAlterForeignKey(options?: UseAlterForeignKeyPropsOptions) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();

  return useMutation<boolean, unknown, AlterForeignKeyProps>({
    ...options,
    mutationFn: async ({ source, ...props }) => {
      const modifyMethods = getDatabaseMethods(source.kind);

      if (!modifyMethods.modify?.alterForeignKey) {
        throw new NotImplementedError(
          `alterForeignKey not implemented for source: ${source.name}`,
        );
      }

      const result = await modifyMethods.modify.alterForeignKey({
        dataSourceName: source.name,
        isMigration: envVars.consoleMode === 'cli',
        endpoints,
        fetchJson,
        ...props,
      });

      return result;
    },
    onSuccess: (data, variables, onMutateResult, context) => {
      hasuraToast({
        title: 'Success!',
        message: 'Foreign key updated successfully!',
        type: 'success',
      });

      invalidateMetadata(queryClient);
      queryClient.invalidateQueries({
        queryKey: [variables.source.name],
      });

      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
  });
}
