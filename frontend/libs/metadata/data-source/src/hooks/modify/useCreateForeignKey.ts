import { useMutation, UseMutationOptions } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource } from '@hasura/shared/types';
import { hasuraToast } from '@hasura/shared/ui';
import {
  getDatabaseMethods,
  ModifyForeignKeyArgs,
  NotImplementedError,
} from '../../driver';

type CreateForeignKeyProps = Omit<ModifyForeignKeyArgs, 'constraintName'> & {
  constraintName?: string;
  source: QualifiedDataSource;
};

type UseCreateForeignKeyPropsOptions = Omit<
  UseMutationOptions<boolean, unknown, CreateForeignKeyProps>,
  'mutationFn'
>;

export function useCreateForeignKey(options?: UseCreateForeignKeyPropsOptions) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useMutation<boolean, unknown, CreateForeignKeyProps>({
    ...options,
    mutationFn: async ({ source, ...props }) => {
      const modifyMethods = getDatabaseMethods(source.kind);

      if (!modifyMethods.modify?.createForeignKey) {
        throw new NotImplementedError(
          `createForeignKey not implemented for source: ${source.name}`,
        );
      }

      const result = await modifyMethods.modify.createForeignKey({
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
        message: 'Foreign key created successfully!',
        type: 'success',
      });

      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
  });
}
