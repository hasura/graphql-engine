import { Metadata, MetadataTableConfig } from '@hasura/shared/types';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import {
  confirmCliServerUnavailable,
  getDownQueryComments,
  getTrackFunctionQuery,
  getTrackTableArgs,
  runDatabaseMigration,
  runMetadataQuery,
  RunSqlArgs,
  runSQLBulk,
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
import {
  hasuraToast,
  showErrorNotification,
  useHasuraAlert,
} from '@hasura/shared/ui';
import {
  escapeTableColumnsMap,
  escapeTableName,
  getDatabaseMethods,
  TableColumn,
} from '@hasura/metadata/data-source';
import { isEmpty } from '@hasura/shared/utils';

type RunSQLProps = RunSqlArgs & {
  isTableTracked: boolean;
  statementTimeout?: number | null;
  migration?: {
    name: string;
  };
};

type UseRunRawSQLOptions = Omit<
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
  // Invoked at render; the captured `hasuraPrompt` callback is only called later
  // inside `runEitherMigrationSQL` (the mutationFn), which the Rules of Hooks
  // allow.
  const { hasuraPrompt } = useHasuraAlert();

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
      const isOk = await confirmCliServerUnavailable(hasuraPrompt);
      if (!isOk) {
        return null;
      }
    }

    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration: true,
      args: {
        name: migration.name,
        up: [
          {
            ...args,
            sql,
            readOnly: isReadOnly,
          },
        ],
        down: getDownQueryComments([sql], args.source).map((query) => ({
          sql: query.args.sql,
        })),
        source: args.source,
      },
    });
  };

  const mutationFn = async (props: RunSQLProps) => {
    const queryResults = await runEitherMigrationSQL(props);

    hasuraToast({
      type: props.isTableTracked ? 'info' : 'success',
      title: 'SQL executed!',
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
          showErrorNotification({
            title: 'Tracking metadata failed!',
            message: 'Something is wrong. Received an invalid response json.',
            error,
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
    const columnConfig: NonNullable<MetadataTableConfig['column_config']> = {};
    configuration.column_config = columnConfig;
    Object.entries(customColumnNames).forEach(([column, columnCustomName]) => {
      columnConfig[column] = { custom_name: columnCustomName };
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
