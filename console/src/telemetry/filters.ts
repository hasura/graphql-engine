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

const filterPayloadAllowList: any[] = [];

const DATA_PATH = '/data';
const API_EXPLORER_PATH = '/api-explorer';
const REMOTE_SCHEMAS_PATH = '/remote-schemas';
const EVENTS_PATH = '/events';

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

const sanitiseUrl = (rawPath: string) => {
  const path = rawPath.replace(new RegExp(globals.urlPrefix, 'g'), '');
  if (path.indexOf(DATA_PATH) === 0) {
    return dataHandler(path.slice(DATA_PATH.length));
  }
  if (path.indexOf(API_EXPLORER_PATH) === 0) {
    return apiExplorerHandler();
  }
  if (path.indexOf(REMOTE_SCHEMAS_PATH) === 0) {
    return remoteSchemasHandler(path.slice(REMOTE_SCHEMAS_PATH.length));
  }
  if (path.indexOf(EVENTS_PATH) === 0) {
    return eventsHandler(path.slice(EVENTS_PATH.length));
  }
  return '/';
};

export { filterEventsBlockList, filterPayloadAllowList, sanitiseUrl };
