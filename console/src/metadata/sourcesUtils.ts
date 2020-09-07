import { Driver } from '../dataSources';

export const addSource = (
  driver: Driver,
  payload: {
    name: string;
    dbUrl: {
      from_env?: string;
      from_value?: string;
    };
    connection_pool_settings?: {
      max_connections?: number;
      connection_idle_timeout?: number; // in seconds
    };
  }
) => {
  const typePrefix = driver === 'postgres' ? 'pg_' : 'mysql_';

  return {
    type: `${typePrefix}add_source`,
    args: {
      name: payload.name,
      url: payload.dbUrl,
      connection_pool_settings: payload.connection_pool_settings,
    },
  };
};

export const removeSource = (driver: Driver, name: string) => {
  const typePrefix = driver === 'postgres' ? 'pg_' : 'mysql_';

  return {
    type: `${typePrefix}remove_source`,
    args: {
      name,
    },
  };
};

export const reloadSource = (driver: Driver, name: string) => {
  const typePrefix = driver === 'postgres' ? 'pg_' : 'mysql_';

  return {
    type: `${typePrefix}reload_source`,
    args: {
      name,
    },
  };
};
