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
  ModifyTableArgs,
  NotImplementedError,
} from '../../driver';
import {
  getTrackTableArgs,
  useErrorNotification,
  useMetadataMigration,
} from '@hasura/metadata/api';

type CreateTableProps = {
  source: QualifiedDataSource;
  args: ModifyTableArgs;
};

type UseCreateTableOptions = Omit<
  UseMutationOptions<boolean, unknown, CreateTableProps>,
  'mutationFn'
>;

export function useCreateTable(options?: UseCreateTableOptions) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();
  const { mutate } = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  return useMutation<boolean, unknown, CreateTableProps>({
    ...options,
    mutationFn: async ({ source, args }) => {
      const modifyMethods = getDatabaseMethods(source.kind);

      if (!modifyMethods.modify?.createTable) {
        throw new NotImplementedError(
          `createTable not implemented for source: ${source.name}`,
        );
      }

      const result = await modifyMethods.modify.createTable({
        dataSourceName: source.name,
        isMigration: envVars.consoleMode === 'cli',
        endpoints,
        fetchJson,
        args,
      });

      if (result) {
        await mutate({
          query: getTrackTableArgs(
            {
              source: source.name,
              table: args.table,
              ...(args.tableComment
                ? { configuration: { comment: args.tableComment } }
                : {}),
            },
            source.kind,
          ),
        });
      }

      return result;
    },
    onSuccess: (data, variables, onMutateResult, context) => {
      hasuraToast({
        title: 'Success!',
        message: 'Table created successfully!',
        type: 'success',
      });

      queryClient.invalidateQueries({
        queryKey: [variables.source.name],
      });

      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
    onError: (error, variables, onMutateResult, context) => {
      showErrorNotification({
        title: 'Creating table failed',
        error,
      });
      options?.onError?.(error, variables, onMutateResult, context);
    },
  });
}
