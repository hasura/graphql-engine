import { Driver, sourceNames } from '../dataSources';
import {
  ConnectionPoolSettings,
  IsolationLevelOptions,
  SourceConnectionInfo,
  SSLConfigOptions,
} from './types';

export const addSource = (
  driver: Driver,
  payload: {
    name: string;
    dbUrl: string | { from_env: string };
    connection_pool_settings?: ConnectionPoolSettings;
    replace_configuration?: boolean;
    bigQuery: {
      projectId: string;
      datasets: string;
      global_select_limit: number;
    };
    sslConfiguration?: SSLConfigOptions;
    preparedStatements?: boolean;
    isolationLevel?: IsolationLevelOptions;
  },
  // supported only for PG sources at the moment
  replicas?: Omit<
    SourceConnectionInfo,
    | 'connection_string'
    | 'use_prepared_statements'
    | 'ssl_configuration'
    | 'isolation_level'
  >[]
) => {
  const replace_configuration = payload.replace_configuration ?? false;
  if (driver === 'mssql') {
    return {
      type: 'mssql_add_source',
      args: {
        name: payload.name,
        configuration: {
          connection_info: {
            connection_string: payload.dbUrl,
            pool_settings: payload.connection_pool_settings,
          },
        },
        replace_configuration,
      },
    };
  }

  if (driver === 'bigquery') {
    const service_account =
      typeof payload.dbUrl === 'string'
        ? JSON.parse(payload.dbUrl)
        : payload.dbUrl;
    return {
      type: 'bigquery_add_source',
      args: {
        name: payload.name,
        configuration: {
          service_account,
          global_select_limit: payload.bigQuery.global_select_limit,
          project_id: payload.bigQuery.projectId,
          datasets: payload.bigQuery.datasets.split(',').map(d => d.trim()),
        },
        replace_configuration,
      },
    };
  }

  return {
    type: `${driver === 'postgres' ? 'pg' : 'citus'}_add_source`,
    args: {
      name: payload.name,
      configuration: {
        connection_info: {
          database_url: payload.dbUrl,
          use_prepared_statements: payload.preparedStatements,
          isolation_level: payload.isolationLevel,
          pool_settings: payload.connection_pool_settings,
          ssl_configuration: payload.sslConfiguration,
        },
        read_replicas: replicas?.length ? replicas : null,
      },
      replace_configuration,
    },
  };
};

export const removeSource = (driver: Driver, name: string) => {
  let prefix = '';
  switch (driver) {
    case sourceNames.mssql:
      prefix = 'mssql_';
      break;
    case sourceNames.bigquery:
      prefix = 'bigquery_';
      break;
    case sourceNames.citus:
      prefix = 'citus_';
      break;
    default:
      prefix = 'pg_';
  }

  return {
    type: `${prefix}drop_source`,
    args: {
      name,
    },
  };
};

export const reloadSource = (name: string) => {
  return {
    type: 'reload_metadata',
    args: {
      reload_sources: [name],
    },
  };
};
