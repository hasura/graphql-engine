import { getSupportedDrivers } from '../../../../../dataSources';
import { DbConnectionSettings } from '../../../../../dataSources/types';
import { ConnectDBState } from '../state';

export const buildFormSettings = (
  dbType: ConnectDBState['dbType']
): DbConnectionSettings => {
  const settings: DbConnectionSettings = {
    connectionSettings: false,
    cumulativeMaxConnections: false,
    retries: false,
    pool_timeout: false,
    connection_lifetime: false,
    isolation_level: false,
    prepared_statements: false,
    ssl_certificates: false,
  };

  let setting: keyof DbConnectionSettings;

  // eslint-disable-next-line no-restricted-syntax, guard-for-in
  for (setting in settings) {
    settings[setting] = getSupportedDrivers(
      `connectDbForm.${setting}`
    ).includes(dbType);
  }
  return settings;
};
