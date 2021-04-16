import { Driver } from '../dataSources';
import { SourceConnectionInfo } from './types';

export const addSource = (
  driver: Driver,
  payload: {
    name: string;
    dbUrl: string | { from_env: string };
    connection_pool_settings?: {
      max_connections?: number;
      idle_timeout?: number;
      retries?: number;
    };
    bigQuery: {
      projectId: string;
      datasets: string;
    };
  },
  // supported only for PG sources at the moment
  replicas?: Omit<SourceConnectionInfo, 'connection_string'>[]
) => {
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
      },
    };
  }

  if (driver === 'bigquery') {
    return {
      type: 'bigquery_add_source',
      args: {
        name: payload.name,
        configuration: {
          service_account: payload.dbUrl,
          project_id: payload.bigQuery.projectId,
          datasets: payload.bigQuery.datasets.split(',').map(d => d.trim()),
        },
      },
    };
  }

  return {
    type: 'pg_add_source',
    args: {
      name: payload.name,
      configuration: {
        connection_info: {
          database_url: payload.dbUrl,
          pool_settings: payload.connection_pool_settings,
        },
        read_replicas: replicas?.length ? replicas : null,
      },
    },
  };
};

export const removeSource = (driver: Driver, name: string) => {
  let prefix = '';
  switch (driver) {
    case 'mssql':
      prefix = 'mssql_';
      break;
    case 'mysql':
      prefix = 'mysql_';
      break;
    case 'bigquery':
      prefix = 'bigquery_';
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
