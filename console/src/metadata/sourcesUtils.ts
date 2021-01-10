import { Driver } from '../dataSources';

export const addSource = (
  driver: Driver,
  payload: {
    name: string;
    dbUrl: string;
    connection_pool_settings?: {
      max_connections?: number;
      idle_timeout?: number; // in seconds
      retries?: number;
    };
  }
) => {
  const typePrefix = driver === 'postgres' ? 'pg_' : 'mysql_';

  return {
    type: `${typePrefix}add_source`,
    args: {
      name: payload.name,
      database_url: payload.dbUrl,
      connection_pool_settings: payload.connection_pool_settings,
    },
  };
};

export const removeSource = (driver: Driver, name: string) => {
  const typePrefix = driver === 'postgres' ? 'pg_' : 'mysql_';

  return {
    type: `${typePrefix}drop_source`,
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
