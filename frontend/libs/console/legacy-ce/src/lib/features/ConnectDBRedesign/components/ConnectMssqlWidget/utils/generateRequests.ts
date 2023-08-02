import { DatabaseConnection } from '../../../types';
import { generateGraphQLCustomizationInfo } from '../../GraphQLCustomization/utils/generateRequest';
import { MssqlConnectionSchema } from '../schema';
import { cleanEmpty } from '../../ConnectPostgresWidget/utils/helpers';

export const generateConnectionInfo = (
  values: MssqlConnectionSchema['configuration']['connectionInfo']
) => {
  return {
    connection_string:
      values.connectionString.connectionType === 'databaseUrl'
        ? values.connectionString.url
        : { from_env: values.connectionString.envVar },
    pool_settings: {
      total_max_connections: values.poolSettings?.totalMaxConnections,
      idle_timeout: values.poolSettings?.idleTimeout,
    },
  };
};

export const generateMssqlRequestPayload = ({
  driver,
  values,
}: {
  driver: string;
  values: MssqlConnectionSchema;
}): DatabaseConnection => {
  const payload = {
    driver,
    details: {
      name: values.name,
      configuration: {
        connection_info: generateConnectionInfo(
          values.configuration.connectionInfo
        ),
        read_replicas: values.configuration.readReplicas?.map(readReplica =>
          generateConnectionInfo(readReplica)
        ),
      },
      customization: generateGraphQLCustomizationInfo(
        values.customization ?? {}
      ),
    },
  };
  return cleanEmpty(payload);
};
