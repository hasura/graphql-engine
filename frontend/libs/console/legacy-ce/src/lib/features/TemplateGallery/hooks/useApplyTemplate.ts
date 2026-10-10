import {
  exportMetadata,
  getReplaceMetadataQuery,
  runDatabaseMigration,
  runMetadataQuery,
  toastMetadataOutOfDateError,
  usePostMetadataMigration,
} from '@hasura/metadata/api';
import {
  TemplateGalleryTemplateDetailFull,
  TemplateGalleryTemplateItem,
} from '../types';
import { useMutation, UseMutationOptions } from '@tanstack/react-query';
import { useAppContext } from '@hasura/shared/context';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { HasuraMetadataV3, QualifiedDataSource } from '@hasura/shared/types';
import { hasuraToast } from '@hasura/shared/ui';

export type ApplyTemplateProps = {
  source: QualifiedDataSource;
  template: TemplateGalleryTemplateItem;
  details: TemplateGalleryTemplateDetailFull;
};

type MutationOptions = UseMutationOptions<unknown, unknown, ApplyTemplateProps>;

const useApplyTemplate = (options?: MutationOptions) => {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const postMetadata = usePostMetadataMigration();

  return useMutation({
    ...options,
    mutationFn: async ({ source, template, details }: ApplyTemplateProps) => {
      await runDatabaseMigration({
        endpoints,
        fetchJson,
        isMigration: envVars.consoleMode === 'cli',
        args: {
          name: `apply_sql_template_${template.key}`,
          source: source,
          up: [
            {
              sql: details.sql,
            },
          ],
          down: [],
        },
      });

      hasuraToast({
        type: 'success',
        title: 'Applying SQL successfully!',
        message:
          'SQL migration successfully applied. Continue to apply metadata from template',
      });

      if (details.metadataObject) {
        const oldMetadata = await exportMetadata({
          url: endpoints.metadata,
          fetchJson,
        });

        const newMetadata: HasuraMetadataV3 = {
          ...oldMetadata.metadata,
          sources: oldMetadata.metadata.sources.map((oldSource) => {
            if (oldSource.name !== source.name) {
              return oldSource;
            }

            const metadataObject =
              details.metadataObject?.metadata?.sources?.[0];
            if (!metadataObject) {
              return oldSource;
            }

            return {
              ...oldSource,
              tables: [...oldSource.tables, ...(metadataObject.tables ?? [])],
              functions: [
                ...(oldSource.functions ?? []),
                ...(metadataObject.functions ?? []),
              ],
            };
          }),
        };

        return runMetadataQuery({
          url: endpoints.metadata,
          fetchJson,
          body: getReplaceMetadataQuery(newMetadata),
        });
      }
    },
    onSuccess: async (data, variables, onMutateResult, context) => {
      await postMetadata(variables);

      hasuraToast({
        type: 'success',
        title: 'Applying metadata successfully!',
        message:
          'SQL migration successfully applied. Continue to apply metadata from template',
      });

      if (options?.onSuccess) {
        options.onSuccess(data, variables, onMutateResult, context);
      }

      return Promise.resolve(data);
    },
    onError: (error, variables, onMutateResult, context) => {
      if (!toastMetadataOutOfDateError(error, () => postMetadata(variables))) {
        hasuraToast({
          type: 'error',
          title: 'Error',
          message: 'An error occurred while applying the template',
        });
      }

      if (options?.onError) {
        options.onError(error, variables, onMutateResult, context);
      }
    },
  });
};

export default useApplyTemplate;
