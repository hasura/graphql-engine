import { useAppContext } from '@hasura/shared/context';
import { useQueryClient } from '@tanstack/react-query';
import {
  invalidateMetadata,
  logMetadataInvalidation,
} from '../useInvalidateMetadata';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { Metadata } from '@hasura/shared/types';

export const usePostMetadataMigration = () => {
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();
  const { endpoints, envVars } = useAppContext();
  const { isProduction } = useAppContext();

  const invalidate = (variables: unknown) => {
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

    if (!isProduction) {
      logMetadataInvalidation({
        componentName: 'useMetadataMigration()',
        reasons: [
          'Metadata migration occurred',
          `Migration Body:`,
          JSON.stringify(variables, null, 2),
        ],
      });
    }

    invalidateMetadata(queryClient);
  };

  return invalidate;
};
