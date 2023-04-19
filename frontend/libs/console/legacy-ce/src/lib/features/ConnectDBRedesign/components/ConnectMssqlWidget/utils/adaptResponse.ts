import { MssqlConfiguration, Source } from '../../../../hasura-metadata-types';
import { adaptGraphQLCustomization } from '../../GraphQLCustomization/utils/adaptResponse';
import { MssqlConnectionInfoSchema, MssqlConnectionSchema } from '../schema';

export const adaptMssqlConnectionInfo = (
  connectionInfo: MssqlConfiguration['connection_info']
): MssqlConnectionInfoSchema => {
  return {
    connectionString:
      typeof connectionInfo.connection_string === 'string'
        ? {
            connectionType: 'databaseUrl',
            url: connectionInfo.connection_string,
          }
        : {
            connectionType: 'envVar',
            envVar: connectionInfo.connection_string.from_env,
          },
    poolSettings: {
      totalMaxConnections:
        connectionInfo.pool_settings?.total_max_connections ?? undefined,
      idleTimeout: connectionInfo.pool_settings?.idle_timeout ?? undefined,
    },
  };
};

export const adaptMssqlConnection = (
  metadataSource: Source
): MssqlConnectionSchema => {
  if (metadataSource.kind !== 'mssql') throw Error('Not a MSSQL connection');

  // This assertion is safe because of the check above.
  const configuration = metadataSource.configuration as MssqlConfiguration;

  return {
    name: metadataSource.name,
    configuration: {
      connectionInfo: adaptMssqlConnectionInfo(configuration.connection_info),
      readReplicas: (configuration.read_replicas ?? []).map(read_replica =>
        adaptMssqlConnectionInfo(read_replica)
      ),
    },
    customization: adaptGraphQLCustomization(
      metadataSource.customization ?? {}
    ),
  };
};
