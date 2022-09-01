/* eslint-disable no-underscore-dangle */
import { isProConsole } from './proConsole';

export const canAccessReadReplica = () => isProConsole(window.__env);

export const canAccessSecuritySettings = () => isProConsole(window.__env);
