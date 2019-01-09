const filterEventsBlockList = [
  'App/ONGOING_REQUEST',
  'App/DONE_REQUEST',
  'App/FAILED_REQUEST',
  'App/ERROR_REQUEST',
];

const filterPayloadAllowList = [];

const dataHandler = path => {
  return (
    'data' +
    path
      .replace(/\/schema\/(\w+)(\/)?/, '/schema/SCHEMA_NAME$2')
      .replace(/(\/schema\/.*)\/tables\/(\w*)(\/.*)?/, '$1/tables/TABLE_NAME$3')
  );
};

const apiExplorerHandler = () => {
  return '/api-explorer';
};

const remoteSchemasHandler = path => {
  return (
    'remote-schemas' +
    path.replace(/(\/manage\/).*(\/\w+.*)$/, '$1REMOTE_SCHEMA_NAME$2')
  );
};

const eventsHandler = path => {
  return (
    'events' +
    path.replace(/(\/manage\/triggers\/).*(\/\w+.*)$/, '$1TRIGGER_NAME$2')
  );
};

const sanitiseUrl = path => {
  if (path.indexOf('/data') === 0) {
    return dataHandler(path.slice(5));
  }
  if (path.indexOf('/api-explorer') === 0) {
    return apiExplorerHandler(path.slice(13));
  }
  if (path.indexOf('/remote-schemas') === 0) {
    return remoteSchemasHandler(path.slice(15));
  }
  if (path.indexOf('/events') === 0) {
    return eventsHandler(path.slice(7));
  }
  return '/';
};

export { filterEventsBlockList, filterPayloadAllowList, sanitiseUrl };
