import { hasuraToast } from '@hasura/shared/ui';
import {
  gqlPattern,
  gqlTableErrorNotif,
  gqlViewErrorNotif,
} from '@hasura/shared/utils';
import { QualifiedDataSource } from '@hasura/shared/types';
import {
  getDatabaseMethods,
  IntrospectedTable,
  NotImplementedError,
} from '../../driver';
import {
  useMutation,
  UseMutationOptions,
  useQueryClient,
} from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { invalidateMetadata, useErrorNotification } from '@hasura/metadata/api';

type ChangeTableNameProps = {
  source: QualifiedDataSource;
  table: Omit<IntrospectedTable, 'name'>;
  newName: string;
};

type UseChangeTableOptions = Omit<
  UseMutationOptions<boolean, unknown, ChangeTableNameProps>,
  'mutationFn'
>;

export const useChangeTableName = (options?: UseChangeTableOptions) => {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();
  const showErrorNotification = useErrorNotification();

  return useMutation({
    ...options,
    mutationFn: async ({ source, table, newName }: ChangeTableNameProps) => {
      const databaseMethods = getDatabaseMethods(source.kind);
      if (!databaseMethods.modify?.changeTableName) {
        throw new NotImplementedError(
          `alterViewComment not implemented for source: ${source.name}`,
        );
      }

      if (!gqlPattern.test(newName)) {
        const gqlValidationError = databaseMethods.check.isTable(table.type)
          ? gqlTableErrorNotif
          : gqlViewErrorNotif;
        hasuraToast({
          type: 'error',
          title: gqlValidationError[3],
          message: gqlValidationError[1],
          children: gqlValidationError[2].custom,
        });

        return false;
      }

      const result = await databaseMethods.modify.changeTableName({
        dataSourceName: source.name,
        table,
        newName,
        endpoints,
        fetchJson,
        isMigration: envVars.consoleMode === 'cli',
      });

      invalidateMetadata(queryClient);
      queryClient.invalidateQueries({
        queryKey: [source.name],
      });

      return result;
    },
    onSuccess: (data, variables, onMutateResult, context) => {
      const property = variables.table.type.toLowerCase();
      hasuraToast({
        message: `Renaming ${property} successful`,
        title: 'Success!',
        type: 'success',
      });

      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
    onError: (err, variables, onMutateResult, context) => {
      const property = variables.table.type.toLowerCase();
      showErrorNotification({
        title: `Renaming ${property} failed!`,
        error: err,
      });
      options?.onError?.(err, variables, onMutateResult, context);
    },
  });
};
