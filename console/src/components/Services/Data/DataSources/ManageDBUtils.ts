import { Driver } from '../../../../dataSources';

export const parseURI = (url: string) => {
  try {
    const pattern = /^(?:([^:/?#\s]+):\/{2})?(?:([^@/?#\s]+)@)?([^/?#\s]+)?(?:\/([^?#\s]*))?(?:[?]([^#\s]+))?\S*$/;
    const matches = url.match(pattern);
    if (!matches) return {};

    const params = {} as Record<string, string | Record<string, string>>;
    if (matches[5] !== undefined) {
      matches[5]?.split('&').forEach((x: string) => {
        const a = x?.split('=');
        if (a[0] && a[1]) params[a[0]] = a[1];
      });
    }

    return {
      protocol: matches[1],
      user: matches[2]?.split(':')[0],
      password: matches[2]?.split(':')[1],
      host: matches[3],
      hostname: matches[3]?.split(/:(?=\d+$)/)[0],
      port: matches[3]?.split(/:(?=\d+$)/)[1],
      segments: matches[4]?.split('/'),
      params,
    };
  } catch (error) {
    console.error(error);
    return {};
  }
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
  if (dbType === 'postgres' || dbType === 'citus') {
    if (!password) {
      return `postgresql://${tUserName}@${tHost}:${tPort}/${tDatabase}`;
    }
    return `postgresql://${tUserName}:${tPassword}@${tHost}:${tPort}/${tDatabase}`;
  }

  // TODO: update this function to work for the other database drivers
  throw new Error('Not implemented for other database drivers');
};
