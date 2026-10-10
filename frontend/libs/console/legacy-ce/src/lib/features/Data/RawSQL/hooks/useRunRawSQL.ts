import { Metadata, MetadataTableConfig } from '@hasura/shared/types';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import {
  getDownQueryComments,
  getRunSqlQuery,
  getTrackFunctionQuery,
  getTrackTableArgs,
  runMetadataQuery,
  RunSqlArgs,
  runSQLBulk,
  runSQLMigrate,
  TMigrationSingleQuery,
  toastMetadataOutOfDateError,
  useInvalidateMetadata,
  useMigrationStatus,
} from '@hasura/metadata/api';
import {
  useMutation,
  UseMutationOptions,
  UseMutationResult,
  useQueryClient,
} from '@tanstack/react-query';
import { HttpError } from '@hasura/shared/types';
import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';
import {
  escapeTableColumnsMap,
  escapeTableName,
  getDatabaseMethods,
  TableColumn,
} from '@hasura/metadata/data-source';
import {
  getConfirmation,
  getErrorMessage,
  isEmpty,
} from '@hasura/shared/utils';

type RunSQLProps = RunSqlArgs & {
  isTableTracked: boolean;
  statementTimeout?: number | null;
  migration?: {
    name: string;
  };
};

export type UseRunRawSQLOptions = Omit<
  UseMutationOptions<string[][], unknown, RunSQLProps>,
  'mutationFn' | 'mutationKey'
>;

/**
 * This run SQL hook is the new implementation of the run sql api using react hooks. Right now, it's used only
 * for gdc based sources since the old rawSQL.js UI needs a rewrite and decoupling from redux
 */
export const useRunRawSQL = (
  options?: UseRunRawSQLOptions,
): UseMutationResult<string[][], unknown, RunSQLProps> => {
  const { endpoints, envVars, readOnlyMode } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();
  const invalidateMetadata = useInvalidateMetadata();
  const { refetch: refetchMigrationStatus } = useMigrationStatus({
    enabled: false,
  });

  const runEitherMigrationSQL = async ({
    statementTimeout,
    migration,
    readOnly,
    sql,
    isTableTracked,
    ...args
  }: RunSQLProps) => {
    const dbMethods = getDatabaseMethods(args.source.kind);
    const isStatementTimeout =
      dbMethods.utilities?.statementTimeoutSQL !== undefined &&
      statementTimeout &&
      statementTimeout > 0 &&
      (envVars.consoleMode === 'server' || !migration);
    const isReadOnly = readOnly || readOnlyMode;

    if (envVars.consoleMode !== 'cli' || !migration) {
      const sqlArgs = {
        ...args,
        sql: isStatementTimeout
          ? [
              dbMethods.utilities?.statementTimeoutSQL?.(statementTimeout) ??
                '',
              sql,
            ].join('\n')
          : sql,
        readOnly: isReadOnly,
      };

      return runSQLBulk({
        url: endpoints.queryV2,
        fetchJson,
        type: 'bulk',
        args: [sqlArgs],
      });
    }

    /*
    During console CLI mode, alert the CLI server to update it's local filesystem after metadata API call is successful
  */
    const cliServerStatus = await refetchMigrationStatus();
    if (cliServerStatus.data !== 'healthy') {
      const isOk = getConfirmation(
        'The CLI server is unable to access. Do you want to continue?',
        true,
      );
      if (!isOk) {
        return null;
      }
    }

    const migrationBody = {
      name: migration.name,
      up: [
        getRunSqlQuery({
          ...args,
          sql,
          readOnly: isReadOnly,
        }),
      ],
      down: getDownQueryComments([sql], args.source),
      datasource: args.source.name,
    };

    return runSQLMigrate({
      url: endpoints.hasuraCliServerMigrate,
      fetchJson,
      body: migrationBody,
    });
  };

  const mutationFn = async (props: RunSQLProps) => {
    const queryResults = await runEitherMigrationSQL(props);

    hasuraToast({
      type: props.isTableTracked ? 'info' : 'success',
      title: 'SQL executed!',
      message: props.isTableTracked
        ? 'Continue to track resources...'
        : undefined,
    });

    const result = queryResults?.[0] ?? null;
    const database = getDatabaseMethods(props.source.kind);

    if (props.isTableTracked && database.utilities.parseCreateSchemaSQL) {
      const objects = database.utilities.parseCreateSchemaSQL(props.sql);
      const changes: TMigrationSingleQuery[] = [];

      const dataSourceAPI = getDatabaseMethods(props.source.kind);

      for (const { type, name, schema, isPartition } of objects) {
        if (isPartition) continue;

        const qualifiedInfo = {
          name,
          schema,
        };
        if (type === 'function') {
          const req = getTrackFunctionQuery(
            {
              function: qualifiedInfo,
              source: props.source.name,
            },
            props.source.kind,
          );
          changes.push(req);
          continue;
        }

        if (!dataSourceAPI) {
          continue;
        }

        const columns = await dataSourceAPI.introspection.getTableColumnInfos({
          endpoints,
          fetchJson,
          dataSourceName: props.source.name,
          table: qualifiedInfo,
        });
        const req = getTrackTableArgs(
          {
            source: props.source.name,
            table: qualifiedInfo,
            configuration: extractTableConfigurationsFromSQL(name, columns),
          },
          props.source.kind,
        );

        changes.push(req);
      }

      if (!changes.length) {
        return result?.result ?? [];
      }

      try {
        await runMetadataQuery({
          url: endpoints.metadata,
          fetchJson,
          body: {
            type: 'bulk',
            args: changes,
          },
        });
        /*
          During console CLI mode, alert the CLI server to update it's local filesystem after metadata API call is successful
          */
        if (envVars.consoleMode === 'cli') {
          queryClient.query({
            queryKey: ['cliExport'],
            queryFn: async () => {
              const cliMetadataExportUrl = `${endpoints.hasuraCliServerMetadata}?export=true`;
              return fetchJson<Metadata>(cliMetadataExportUrl);
            },
          });
        }

        invalidateMetadata({
          componentName: 'useRunRawSQL()',
          reasons: ['Metadata migration occurred'],
        });

        hasuraToast({
          type: 'success',
          title: 'Tracked resources successfully!',
        });
      } catch (error) {
        if (!toastMetadataOutOfDateError(error, invalidateMetadata)) {
          hasuraToast({
            type: 'error',
            title: 'Tracking metadata failed!',
            message: 'Something is wrong. Received an invalid response json.',
            children:
              error instanceof HttpError && error.data
                ? JSON.stringify(error.data)
                : getErrorMessage(error),
          });
        }
      }
    }

    return result?.result ?? [];
  };

  return useMutation({
    ...options,
    mutationFn,
    onError: (error) => {
      showErrorNotification({
        title: 'Executing SQL failed',
        error,
      });
    },
  });
};

function extractTableConfigurationsFromSQL(
  name: string,
  columns: TableColumn[],
) {
  const configuration: MetadataTableConfig = {};

  const customColumnNames = escapeTableColumnsMap(columns);
  if (!isEmpty(customColumnNames)) {
    configuration.column_config = {};
    Object.entries(customColumnNames).forEach(([column, columnCustomName]) => {
      configuration.column_config![column] = { custom_name: columnCustomName };
    });
  }

  const customName = escapeTableName(name);
  if (customName) {
    configuration.custom_name = customName;
  }

  if (Object.keys(configuration).length === 0) {
    return undefined;
  }

  return configuration;
}
