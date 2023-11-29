import { DatabaseConnection } from '../../../types';
import { generateGraphQLCustomizationInfo } from '../../GraphQLCustomization/utils/generateRequest';
import { PostgresConnectionSchema } from '../schema';
import { cleanEmpty } from './helpers';

export const generateConnectionInfo = (
  values: PostgresConnectionSchema['configuration']['connectionInfo']
) => ({
  database_url:
    values.databaseUrl.connectionType === 'databaseUrl'
      ? values.databaseUrl.url
      : values.databaseUrl.connectionType === 'envVar'
      ? { from_env: values.databaseUrl.envVar }
      : values.databaseUrl.connectionType === 'dynamicFromFile'
      ? { dynamic_from_file: values.databaseUrl.dynamicFromFile }
      : {
          connection_parameters: {
            username: values.databaseUrl.username,
            password: values.databaseUrl.password,
            database: values.databaseUrl.database,
            port: values.databaseUrl.port,
            host: values.databaseUrl.host,
          },
        },
  pool_settings: {
    total_max_connections: values.poolSettings?.totalMaxConnections,
    idle_timeout: values.poolSettings?.idleTimeout,
    retries: values.poolSettings?.retries,
    pool_timeout: values.poolSettings?.poolTimeout,
    connection_lifetime: values.poolSettings?.connectionLifetime,
  },
  use_prepared_statements: values.usePreparedStatements,
  isolation_level: values.isolationLevel,
  ssl_configuration: {
    sslmode: values.sslSettings?.sslMode,
    sslrootcert: {
      from_env: values.sslSettings?.sslRootCert,
    },
    sslcert: {
      from_env: values.sslSettings?.sslCert,
    },
    sslkey: {
      from_env: values.sslSettings?.sslKey,
    },
    sslpassword: {
      from_env: values.sslSettings?.sslPassword,
    },
  },
});

export const generatePostgresRequestPayload = ({
  driver,
  values,
}: {
  driver: string;
  values: PostgresConnectionSchema;
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
        extensions_schema: values.configuration.extensionSchema,
      },
      customization: generateGraphQLCustomizationInfo(
        values.customization ?? {}
      ),
    },
  };
  return cleanEmpty(payload);
};
