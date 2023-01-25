import { CapabilitiesResponse } from '@hasura/dc-api-types';
import { AxiosInstance } from 'axios';
import { runMetadataQuery } from '../../api';

export const getDatabaseConfiguration = async (
  httpClient: AxiosInstance,
  driver?: string
) => {
  if (!driver) throw Error('getDatabaseConfiguration: driver is undefined');

  const result = await runMetadataQuery<{
    capabilities: CapabilitiesResponse['capabilities'];
    config_schema_response: CapabilitiesResponse['config_schemas'];
    options: {
      uri: string;
    };
  }>({
    httpClient,
    body: {
      type: 'get_source_kind_capabilities',
      args: {
        name: driver,
      },
    },
  });

  return {
    configSchema: result.config_schema_response.config_schema,
    otherSchemas: result.config_schema_response.other_schemas,
  };
};
