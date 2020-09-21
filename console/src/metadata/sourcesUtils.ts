import { Driver } from '../dataSources';

export const addSource = (
  driver: Driver,
  payload: {
    name: string;
    dbUrl: string;
    connection_pool_setting?: {
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
      database_url: payload.dbUrl,
      connection_pool_setting: payload.connection_pool_setting,
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
