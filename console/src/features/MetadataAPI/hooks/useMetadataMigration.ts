import { CLI_CONSOLE_MODE, SERVER_CONSOLE_MODE } from '@/constants';
import Endpoints from '@/Endpoints';
import { useMigrationMode } from '@/hooks';
import { Api } from '@/hooks/apiUtils';
import { RunSQLResponse } from '@/hooks/types';
import { useConsoleConfig } from '@/hooks/useEnvVars';
import { useAppSelector } from '@/store';
import { useMutation, UseMutationOptions, useQueryClient } from 'react-query';
import sanitize from 'sanitize-filename';
import { allowedMetadataTypes, MetadataResponse } from '../types';
import { returnMigrateUrl } from './utils';

const maxAllowedLength = 255;
const unixEpochLength = 14;
export const maxAllowedMigrationLength = maxAllowedLength - unixEpochLength;

export type TMigration = {
  source: string;
  query: { type: allowedMetadataTypes; args: Record<string, any> };
  migrationName: string;
};

export function useMetadataMigration(
  mutationOptions?: Omit<
    UseMutationOptions<Record<string, any>, Error, TMigration>,
    'mutationFn'
  >,
  overrideCliMode?: boolean
) {
  const { mode } = useConsoleConfig();
  const headers = useAppSelector(state => state.tables.dataHeaders);

  const { data: migrationMode } = useMigrationMode();
  const queryClient = useQueryClient();
  return useMutation(
    async props => {
      try {
        const { source, query, migrationName } = props;

        const migrateUrl = returnMigrateUrl(
          migrationMode ?? false,
          [query],
          overrideCliMode
        );

        let body = {};

        if (mode === SERVER_CONSOLE_MODE || overrideCliMode) {
          body = query;
        } else {
          body = {
            name: sanitize(
              migrationName.substring(0, maxAllowedMigrationLength)
            ),
            up: [query],
            down: [],
            datasource: source,
            skip_execution: false,
          };
        }

        const result = await Api.post<RunSQLResponse>({
          url: migrateUrl,
          headers,
          body,
        });

        return result;
      } catch (err) {
        throw err;
      }
    },
    {
      ...mutationOptions,
      onSuccess: (data, variables, ctx) => {
        if (mode === CLI_CONSOLE_MODE) {
          queryClient.refetchQueries('migrationMode', { active: true });
          queryClient.fetchQuery({
            queryKey: 'cliExport',
            queryFn: () => {
              const cliMetadataExportUrl = `${Endpoints.hasuractlMetadata}?export=true`;
              return Api.get<MetadataResponse>({
                headers,
                url: cliMetadataExportUrl,
              });
            },
          });
        }

        queryClient.refetchQueries(['metadata'], { active: true });

        const { onSuccess } = mutationOptions ?? {};
        if (onSuccess) {
          onSuccess(data, variables, ctx);
        }
      },
    }
  );
}
