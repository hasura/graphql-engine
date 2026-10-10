import { useMutation, UseMutationOptions } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { FetchJson, getConfirmation } from '@hasura/shared/utils';
import sanitize from 'sanitize-filename';
import { MAX_ALLOWED_MIGRATION_LENGTH } from '../constants';
import { Endpoints, useAppContext } from '@hasura/shared/context';
import {
  getMigrationStatus,
  getRunSqlQuery,
  RunSqlArgs,
  runSQLBulk,
  runSQLMigrate,
} from '../../api';
import { QualifiedDataSource, RunSQLResponse } from '@hasura/shared/types';

export type DatabaseMigrationOptions = Omit<
  UseMutationOptions<RunSQLResponse[] | null, unknown, TDatabaseMigration>,
  'mutationFn'
>;

export type RunDatabaseMigrationArgs = Omit<RunSqlArgs, 'source'>;

export type TDatabaseMigration = {
  name: string;
  up: RunDatabaseMigrationArgs[];
  down: RunDatabaseMigrationArgs[];
  source: QualifiedDataSource;
  skip_execution?: boolean;
};

export type RunDatabaseMigrationProps = {
  endpoints: Endpoints;
  fetchJson: FetchJson;
  args: TDatabaseMigration;
  isMigration: boolean;
};

export async function runDatabaseMigration({
  endpoints,
  fetchJson,
  args,
  isMigration,
}: RunDatabaseMigrationProps) {
  if (!isMigration) {
    return runSQLBulk({
      url: endpoints.queryV2,
      fetchJson,
      args: args.up.map((up) => ({
        ...up,
        source: args.source,
      })),
    });
  }

  const migrationBody = {
    name: sanitize(args.name.substring(0, MAX_ALLOWED_MIGRATION_LENGTH)),
    up: args.up.map((up) =>
      getRunSqlQuery({
        ...up,
        source: args.source,
      }),
    ),
    down: args.down.map((down) =>
      getRunSqlQuery({
        ...down,
        source: args.source,
      }),
    ),
    datasource: args.source.name,
    skip_execution: args.skip_execution,
  };

  return runSQLMigrate({
    url: endpoints.hasuraCliServerMigrate,
    fetchJson,
    body: migrationBody,
  });
}

export function useDatabaseMigration(
  mutationOptions: DatabaseMigrationOptions = {},
) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useMutation({
    mutationFn: async (props) => {
      const isMigration = envVars.consoleMode === 'cli';
      if (isMigration) {
        /*
      During console CLI mode, alert the CLI server to update it's local filesystem after metadata API call is successful
      */
        const cliServerStatus = await getMigrationStatus(
          endpoints.hasuraCliServerMigrateSettings,
        );
        if (cliServerStatus !== 'healthy') {
          const isOk = getConfirmation(
            'The CLI server is unable to access. Do you want to continue?',
            true,
          );
          if (!isOk) {
            return null;
          }
        }
      }

      return runDatabaseMigration({
        isMigration,
        endpoints,
        fetchJson,
        args: props,
      });
    },
    ...mutationOptions,
  });
}
