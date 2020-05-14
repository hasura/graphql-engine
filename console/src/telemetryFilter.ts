import globals from './Globals';

const filterEventsBlockList = [
  'App/ONGOING_REQUEST',
  'App/DONE_REQUEST',
  'App/FAILED_REQUEST',
  'App/ERROR_REQUEST',
  'RNS_SHOW_NOTIFICATION',
  'RNS_HIDE_NOTIFICATION',
  'RNS_REMOVE_ALL_NOTIFICATIONS',
];

const DATA_PATH = '/data' as const;
const API_EXPLORER_PATH = '/api-explorer' as const;
const REMOTE_SCHEMAS_PATH = '/remote-schemas' as const;
const EVENTS_PATH = '/events' as const;

const dataHandler = (path: string) => {
  return (
    DATA_PATH +
    path
      .replace(/\/schema\/([^/]*)(\/)?/, '/schema/SCHEMA_NAME$2')
      .replace(
        /(\/schema\/.*)\/tables\/([^/]*)(\/.*)?/,
        '$1/tables/TABLE_NAME$3'
      )
      .replace(/(\/schema\/.*)\/views\/([^/]*)(\/.*)?/, '$1/views/VIEW_NAME$3')
      .replace(
        /(\/schema\/.*)\/functions\/([^/]*)(\/.*)?/,
        '$1/functions/FUNCTION_NAME$3'
      )
  );
};

const apiExplorerHandler = () => {
  return API_EXPLORER_PATH;
};

const remoteSchemasHandler = (path: string) => {
  return (
    REMOTE_SCHEMAS_PATH +
    path.replace(/(\/manage\/)[^/]*(\/\w+.*)$/, '$1REMOTE_SCHEMA_NAME$2')
  );
};

const eventsHandler = (path: string) => {
  return (
    EVENTS_PATH +
    path.replace(/(\/manage\/triggers\/)[^/]*(\/\w+.*)$/, '$1TRIGGER_NAME$2')
  );
};

const sanitiseUrl = (path: string) => {
  const newPath = path.replace(new RegExp(globals.urlPrefix, 'g'), '');
  if (newPath.indexOf(DATA_PATH) === 0) {
    return dataHandler(newPath.slice(DATA_PATH.length));
  }
  if (newPath.indexOf(API_EXPLORER_PATH) === 0) {
    return apiExplorerHandler();
  }
  if (newPath.indexOf(REMOTE_SCHEMAS_PATH) === 0) {
    return remoteSchemasHandler(newPath.slice(REMOTE_SCHEMAS_PATH.length));
  }
  if (newPath.indexOf(EVENTS_PATH) === 0) {
    return eventsHandler(newPath.slice(EVENTS_PATH.length));
  }
  return '/';
};

export { filterEventsBlockList, sanitiseUrl };
