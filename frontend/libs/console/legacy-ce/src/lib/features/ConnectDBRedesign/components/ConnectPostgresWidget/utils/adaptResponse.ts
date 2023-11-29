import {
  PostgresConfiguration,
  Source,
} from '../../../../hasura-metadata-types';
import { adaptGraphQLCustomization } from '../../GraphQLCustomization/utils/adaptResponse';
import {
  PostgresConnectionInfoSchema,
  PostgresConnectionSchema,
} from '../schema';

export const adaptDatabaseUrl = (
  databaseUrl: PostgresConfiguration['connection_info']['database_url']
): PostgresConnectionInfoSchema['databaseUrl'] => {
  return typeof databaseUrl === 'string'
    ? {
        connectionType: 'databaseUrl',
        url: databaseUrl,
      }
    : 'from_env' in databaseUrl
    ? {
        connectionType: 'envVar',
        envVar: databaseUrl.from_env,
      }
    : 'dynamic_from_file' in databaseUrl
    ? {
        connectionType: 'dynamicFromFile',
        dynamicFromFile: databaseUrl.dynamic_from_file,
      }
    : {
        connectionType: 'connectionParams',
        host: databaseUrl.host,
        password: databaseUrl.password,
        database: databaseUrl.database,
        port: parseInt(databaseUrl.port),
        username: databaseUrl.username,
      };
};

export const adaptPostgresConnectionInfo = (
  connectionInfo: PostgresConfiguration['connection_info']
): PostgresConnectionInfoSchema => {
  return {
    databaseUrl: adaptDatabaseUrl(connectionInfo.database_url),
    poolSettings: {
      totalMaxConnections: connectionInfo.pool_settings?.total_max_connections,
      idleTimeout: connectionInfo.pool_settings?.idle_timeout,
      retries: connectionInfo.pool_settings?.retries,
      poolTimeout: connectionInfo.pool_settings?.pool_timeout,
      connectionLifetime: connectionInfo.pool_settings?.connection_lifetime,
    },
    usePreparedStatements: connectionInfo.use_prepared_statements,
    isolationLevel: connectionInfo.isolation_level,
    sslSettings: {
      sslMode: connectionInfo.ssl_configuration?.sslmode,
      sslRootCert: connectionInfo.ssl_configuration?.sslrootcert?.from_env,
      sslCert: connectionInfo.ssl_configuration?.sslcert?.from_env,
      sslKey: connectionInfo.ssl_configuration?.sslkey?.from_env,
      sslPassword: connectionInfo.ssl_configuration?.sslpassword?.from_env,
    },
  };
};

export const adaptPostgresConnection = (
  metadataSource: Source
): PostgresConnectionSchema => {
  // This assertion is safe because of the check above.
  const configuration = metadataSource.configuration as PostgresConfiguration;

  return {
    name: metadataSource.name,
    configuration: {
      connectionInfo: adaptPostgresConnectionInfo(
        configuration.connection_info
      ),
      readReplicas: (configuration.read_replicas ?? []).map(read_replica =>
        adaptPostgresConnectionInfo(read_replica)
      ),
      extensionSchema: configuration.extensions_schema,
    },
    customization: adaptGraphQLCustomization(
      metadataSource.customization ?? {}
    ),
  };
};
