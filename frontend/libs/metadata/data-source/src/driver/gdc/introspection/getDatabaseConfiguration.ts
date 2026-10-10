import { CapabilitiesResponse } from '@hasura/dc-api-types';
import { runMetadataQuery } from '@hasura/metadata/api';
import { GetDatabaseConfigurationProps } from '../../types';

export const getSourceKindCapabilities = async ({
  driver,
  endpoints,
  fetchJson,
}: GetDatabaseConfigurationProps) => {
  return runMetadataQuery<CapabilitiesResponse>({
    url: endpoints.metadata,
    fetchJson,
    body: {
      type: 'get_source_kind_capabilities',
      args: {
        name: driver,
      },
    },
  });
};

export const getDatabaseConfiguration = async (
  args: GetDatabaseConfigurationProps,
) => {
  const result = await getSourceKindCapabilities(args);

  return {
    configSchema: result.config_schemas.config_schema,
    otherSchemas: result.config_schemas.other_schemas,
  };
};
