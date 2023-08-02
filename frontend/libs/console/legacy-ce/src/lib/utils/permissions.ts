/* eslint-disable no-underscore-dangle */
import { Driver } from '../dataSources';
import { isProConsole } from './proConsole';

export const canAccessReadReplica = (dbType: Driver) => {
  return isProConsole(window.__env) && dbType !== 'bigquery';
};

export const canAccessSecuritySettings = () => isProConsole(window.__env);

export const canAccessCacheButton = () => isProConsole(window.__env);
