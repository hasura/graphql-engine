import {
  CapabilitiesResponse,
  ConfigSchemaResponse,
} from '@hasura/dc-api-types';
import { runMetadataQuery } from '@hasura/metadata/api';
import { GetDatabaseConfigurationProps } from '../../types';

export const getSourceKindCapabilities = async ({
  driver,
  endpoints,
  fetchJson,
}: GetDatabaseConfigurationProps) => {
  const result = await runMetadataQuery<CapabilitiesResponse>({
    url: endpoints.metadata,
    fetchJson,
    body: {
      type: 'get_source_kind_capabilities',
      args: {
        name: driver,
      },
    },
  });

  if (result.config_schemas) {
    return result;
  }

  // Fallback to config_schema_response field for backward compatibility.
  if ('config_schema_response' in result && result.config_schema_response) {
    result.config_schemas =
      result.config_schema_response as ConfigSchemaResponse;
  }

  return result;
};

export const getDatabaseConfiguration = async (
  args: GetDatabaseConfigurationProps,
) => {
  const result = await getSourceKindCapabilities(args);

  return {
    configSchema: result.config_schemas?.config_schema,
    otherSchemas: result.config_schemas?.other_schemas,
  };
};
