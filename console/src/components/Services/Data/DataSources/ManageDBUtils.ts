import { DataSource } from '../../../../metadata/types';
import { Driver } from '../../../../dataSources';

export const getHostFromConnectionString = (datasource: DataSource) => {
  const connectionString =
    typeof datasource.url === 'string'
      ? datasource.url
      : datasource.url.from_env;
  // this is for postgres
  if (
    connectionString.includes('postgresql') ||
    connectionString.indexOf('postgresql') !== -1
  ) {
    return connectionString.split('@')[1].split(':')[0];
  }
  // TODO: update this function with connection string for other databases
  return null;
};

export const makeConnectionStringFromConnectionParams = ({
  dbType,
  host,
  port,
  username,
  database,
  password,
}: {
  dbType: Driver;
  host: string;
  port: string;
  username: string;
  database: string;
  password?: string;
}) => {
  // trim inputs
  const tHost = host.trim();
  const tPort = port.trim();
  const tUserName = username.trim();
  const tDatabase = database.trim();
  let tPassword = '';
  if (password) {
    tPassword = password.trim();
  }
  if (dbType === 'postgres') {
    if (!password) {
      return `postgresql://${tUserName}@${tHost}:${tPort}/${tDatabase}`;
    }
    return `postgresql://${tUserName}:${tPassword}@${tHost}:${tPort}/${tDatabase}`;
  }

  // TODO: update this function to work for the other database drivers
  throw new Error('Not implemented for other database drivers');
};
