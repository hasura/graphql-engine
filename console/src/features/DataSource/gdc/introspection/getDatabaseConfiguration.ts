import { CapabilitiesResponse } from '@hasura/dc-api-types';
import { AxiosInstance } from 'axios';
import { runMetadataQuery } from '../../api';
import { Property } from '../../types';

export const getDatabaseConfiguration = async (
  httpClient: AxiosInstance,
  driver?: string
) => {
  if (!driver) throw Error('getDatabaseConfiguration: driver is undefined');

  const result = await runMetadataQuery<{
    capabilities: CapabilitiesResponse['capabilities'];
    config_schema_response: CapabilitiesResponse['configSchemas'];
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

  return result.config_schema_response as {
    configSchema: Property;
    otherSchemas: Record<string, Property>;
  };
};
