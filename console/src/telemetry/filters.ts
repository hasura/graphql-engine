import globals from '../Globals';

const filterEventsBlockList = [
  'App/ONGOING_REQUEST',
  'App/DONE_REQUEST',
  'App/FAILED_REQUEST',
  'App/ERROR_REQUEST',
  'RNS_SHOW_NOTIFICATION',
  'RNS_HIDE_NOTIFICATION',
  'RNS_REMOVE_ALL_NOTIFICATIONS',
];

const DATA_PATH = '/data';
const REMOTE_SCHEMAS_PATH = '/remote-schemas';
const EVENTS_PATH = '/events';
const ACTIONS_PATH = '/actions';

const dataHandler = (path: string) => {
  return path
    .replace(/(\/schema\/)[^/]*(\/)?/, '$1SCHEMA_NAME$2')
    .replace(/(\/schema\/.*\/tables\/)[^/]*(\/.*)?/, '$1TABLE_NAME$2')
    .replace(/(\/schema\/.*\/views\/)[^/]*(\/.*)?/, '$1VIEW_NAME$2')
    .replace(/(\/schema\/.*\/functions\/)[^/]*(\/.*)?/, '$1FUNCTION_NAME$2');
};

const remoteSchemasHandler = (path: string) => {
  return path.replace(/(\/manage\/)[^/]*(\/\w+.*)$/, '$1REMOTE_SCHEMA_NAME$2');
};

const eventsHandler = (path: string) => {
  return path
    .replace(/(\/manage\/triggers\/)[^/]*(\/\w+.*)$/, '$1TRIGGER_NAME$2')
    .replace(/(\/data\/)[^/]*\/(.*)+$/, '$1DATA_TRIGGER_NAME/$2')
    .replace(/(\/cron\/)[^/]*\/(.*)+$/, '$1CRON_TRIGGER_NAME/$2');
};

const actionsHandler = (path: string) => {
  return path.replace(/(\/manage\/)[^/]*\/(.*)+$/, '$ACTION_NAME/$2');
};

const sanitiseUrl = (rawPath: string) => {
  const path = rawPath.replace(new RegExp(globals.urlPrefix, 'g'), '');
  if (path.startsWith(DATA_PATH)) {
    return dataHandler(path);
  }
  if (path.startsWith(REMOTE_SCHEMAS_PATH)) {
    return remoteSchemasHandler(path);
  }
  if (path.startsWith(EVENTS_PATH)) {
    return eventsHandler(path);
  }
  if (path.startsWith(ACTIONS_PATH)) {
    return actionsHandler(path);
  }
  return path;
};

export { filterEventsBlockList, sanitiseUrl };
