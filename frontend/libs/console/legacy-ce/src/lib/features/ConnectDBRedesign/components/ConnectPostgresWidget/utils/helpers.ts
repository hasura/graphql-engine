import { isProConsole } from '../../../../../utils';
import { PostgresConnectionInfoSchema } from '../schema';

export const getDatabaseConnectionDisplayName = (
  databaseUrl: PostgresConnectionInfoSchema['databaseUrl']
) => {
  return databaseUrl.connectionType === 'databaseUrl'
    ? databaseUrl.url
    : databaseUrl.connectionType === 'envVar'
    ? databaseUrl.envVar
    : databaseUrl.connectionType === 'dynamicFromFile'
    ? databaseUrl.dynamicFromFile
    : getDatabaseUrlFromConnectionParams({
        username: databaseUrl.username,
        password: databaseUrl.password,
        database: databaseUrl.database,
        host: databaseUrl.host,
        port: databaseUrl.port,
      });
};

export const getDatabaseUrlFromConnectionParams = ({
  username,
  password,
  database,
  host,
  port,
}: {
  username: string;
  password?: string;
  database: string;
  host: string;
  port: number;
}) => `postgresql://${username}.${password ?? ''}@${host}:${port}/${database}`;

const isFalseyValue = (v: any) =>
  Number.isNaN(v) ||
  v === undefined ||
  v === null ||
  (typeof v === 'object' && Object.keys(v).length === 0) ||
  (typeof v === 'string' && v.length === 0);

export const cleanEmpty = (obj: Record<string, any>): any => {
  if (Array.isArray(obj)) {
    return obj
      .map(v => (v && typeof v === 'object' ? cleanEmpty(v) : v))
      .filter(v => !isFalseyValue(v));
  } else {
    return Object.entries(obj)
      .map(([k, v]) => [k, v && typeof v === 'object' ? cleanEmpty(v) : v])
      .reduce(
        (a, [k, v]) => (isFalseyValue(v) ? a : ((a as any)[k] = v), a),
        {}
      );
  }
};

export const areSSLSettingsEnabled = () => {
  return isProConsole(window.__env);
};

export const areReadReplicasEnabled = () => {
  return isProConsole(window.__env) || true;
};
