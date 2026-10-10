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
  DropColumnArgs,
  getDatabaseMethods,
  NotImplementedError,
} from '../../driver';
import { invalidateMetadata } from '@hasura/metadata/api';

type Props = DropColumnArgs & { source: QualifiedDataSource };

type Options = Omit<UseMutationOptions<boolean, unknown, Props>, 'mutationFn'>;

export function useDropColumn(options?: Options) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();

  return useMutation<boolean, unknown, Props>({
    ...options,
    mutationFn: async ({ source, ...props }) => {
      const modifyMethods = getDatabaseMethods(source.kind);
      if (!modifyMethods.modify?.dropColumn) {
        throw new NotImplementedError(
          `dropColumn not implemented for source: ${source.name}`,
        );
      }
      return modifyMethods.modify.dropColumn({
        dataSourceName: source.name,
        isMigration: envVars.consoleMode === 'cli',
        endpoints,
        fetchJson,
        ...props,
      });
    },
    onSuccess: (data, variables, onMutateResult, context) => {
      hasuraToast({
        title: 'Success!',
        message: 'Column removed successfully!',
        type: 'success',
      });
      invalidateMetadata(queryClient);
      queryClient.invalidateQueries({ queryKey: [variables.source.name] });
      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
  });
}
