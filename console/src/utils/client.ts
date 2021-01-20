import globals from '../Globals';
import { SERVER_CONSOLE_MODE } from '../constants';

export const getClientTypeHeader = () => {
  let currentServerType = 'oss';
  if (globals.serverVersion.includes('pro')) {
    currentServerType = 'pro';
  } else if (globals.serverVersion.includes('cloud')) {
    currentServerType = 'cloud';
  }

  if (globals.consoleMode === SERVER_CONSOLE_MODE) {
    return `hasura-console-server-${currentServerType}`;
  }

  // it's on the CLI
  if (currentServerType === 'oss') {
    return 'hasura-console-cli-oss';
  }

  // there's only the pro-cli
  return 'hasura-console-cli-pro';
};

export const requestTagHeader = 'Hasura-Internal-Request-Source';
