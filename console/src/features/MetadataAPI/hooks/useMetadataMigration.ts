import { CLI_CONSOLE_MODE } from '@/constants';
import Endpoints from '@/Endpoints';
import { Api } from '@/hooks/apiUtils';
import { RunSQLResponse } from '@/hooks/types';
import { useConsoleConfig } from '@/hooks/useEnvVars';
import { useMutation, UseMutationOptions, useQueryClient } from 'react-query';
import { useSelector } from 'react-redux';
import { allowedMetadataTypes, MetadataResponse } from '../types';

const maxAllowedLength = 255;
const unixEpochLength = 14;
export const maxAllowedMigrationLength = maxAllowedLength - unixEpochLength;

export type TMigration = {
  query: {
    type: allowedMetadataTypes;
    args: Record<string, any>;
    resource_version?: number;
  };
};

export function useMetadataMigration(
  mutationOptions?: Omit<
    UseMutationOptions<Record<string, any>, Error, TMigration>,
    'mutationFn'
  >
) {
  const { mode } = useConsoleConfig();
  // Needed to avoid circular dependency
  const headers = useSelector<any>(state => state.tables.dataHeaders) as Record<
    string,
    string
  >;
  const queryClient = useQueryClient();
  return useMutation(
    async props => {
      try {
        const { query } = props;
        const body = query;
        const result = await Api.post<RunSQLResponse>({
          url: Endpoints.metadata,
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
        /* 
          During console CLI mode, alert the CLI server to update it's local filesystem after metadata API call is successfull
        */
        if (mode === CLI_CONSOLE_MODE) {
          queryClient.refetchQueries('migrationMode', { active: true });
          queryClient.fetchQuery({
            queryKey: 'cliExport',
            queryFn: () => {
              const cliMetadataExportUrl = `${Endpoints.hasuraCliServerMetadata}?export=true`;
              return Api.get<MetadataResponse>({
                headers,
                url: cliMetadataExportUrl,
              });
            },
          });
        }

        /* 
          Get the latest metadata from server (this will NOT update metadata that is in the redux state, to do that please pass a custom onSuccess)
        */
        queryClient.refetchQueries(['metadata'], { active: true });

        const { onSuccess } = mutationOptions ?? {};
        if (onSuccess) {
          onSuccess(data, variables, ctx);
        }
      },
    }
  );
}
