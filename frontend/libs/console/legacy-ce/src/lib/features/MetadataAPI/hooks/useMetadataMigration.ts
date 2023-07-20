import { useMutation, UseMutationOptions, useQueryClient } from 'react-query';
import { useSelector } from 'react-redux';
import { CLI_CONSOLE_MODE } from '../../../constants';
import Endpoints from '../../../Endpoints';
import { Api } from '../../../hooks/apiUtils';
import { useConsoleConfig } from '../../../hooks/useEnvVars';
import { useInvalidateMetadata } from '../../hasura-metadata-api';
import { allowedMetadataTypes, MetadataResponse } from '../types';

const maxAllowedLength = 255;
const unixEpochLength = 14;
export const MAX_METADATA_BATCH_SIZE = 5;

export const maxAllowedMigrationLength = maxAllowedLength - unixEpochLength;

export type TMigration<
  ArgsType extends Record<string, any> = Record<string, any>
> = {
  query: {
    type: allowedMetadataTypes;
    args: ArgsType;
    resource_version?: number;
    source?: string;
  };
};

export type MetadataMigrationOptions<
  // So, I want some validation on this change from others. The type being used as the return from the metadata query was "RunSQLResponse".
  // This type was not correct for dc_add_agent, and the closer I looked the return seems variable based on the query type.
  // I checked every reference to useMetadataMigration and it did not appear there was anywhere in the code using the onSuccess arguments, so this change seems safe.
  // So, I thought the most flexible way to approach this would be to change the base type to Record<string, any> and allow a dev to pass more specific types if needed.
  // I also added and ArgsType so that the args will be typed if the dev would prefer.
  // after review I can modify this comment and remove the explanation of change.
  ResponseType extends Record<string, any> = Record<string, any>,
  ArgsType extends Record<string, any> = Record<string, any>
> = Omit<
  UseMutationOptions<ResponseType, Error, TMigration<ArgsType>>,
  'mutationFn'
> & {
  errorTransform?: (error: unknown) => unknown;
};

export function useMetadataMigration<
  ResponseType extends Record<string, any> = Record<string, any>,
  ArgsType extends Record<string, any> = Record<string, any>
>(
  metadataMigrationOptions: MetadataMigrationOptions<
    ResponseType,
    ArgsType
  > = {},
  additionalQueryKeysToInvalidate?: string[]
) {
  const { errorTransform, ...mutationOptions } = metadataMigrationOptions;

  const { mode } = useConsoleConfig();
  // Needed to avoid circular dependency
  const headers = useSelector<any>(state => state.tables.dataHeaders) as Record<
    string,
    string
  >;
  const queryClient = useQueryClient();
  const invalidateMetadata = useInvalidateMetadata();
  let lastBody: any = null;

  return useMutation(
    async props => {
      const { query } = props;
      const body = query;
      lastBody = body;
      const result = await Api.post<ResponseType>(
        {
          url: Endpoints.metadata,
          headers,
          body,
        },
        undefined,
        errorTransform
      );

      return result;
    },
    {
      ...mutationOptions,
      onSuccess: (data, variables, ctx) => {
        /*
          During console CLI mode, alert the CLI server to update it's local filesystem after metadata API call is successful
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

        invalidateMetadata({
          componentName: 'useMetadataMigration()',
          reasons: [
            'Metadata migration occurred',
            `Migration Type: ${lastBody.type}`,
            `Migration Body:`,
            JSON.stringify(lastBody, null, 2),
          ],
          additionalQueryKeys: additionalQueryKeysToInvalidate?.length
            ? additionalQueryKeysToInvalidate
            : undefined,
        });

        const { onSuccess } = mutationOptions ?? {};
        if (onSuccess) {
          onSuccess(data, variables, ctx);
        }
      },
    }
  );
}
