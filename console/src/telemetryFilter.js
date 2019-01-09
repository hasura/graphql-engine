const filterEventsBlockList = [
  'App/ONGOING_REQUEST',
  'App/DONE_REQUEST',
  'App/FAILED_REQUEST',
  'App/ERROR_REQUEST',
];

const filterPayloadAllowList = [
  'ViewTable/FilterQuery/SET_FILTEROP',
  'ViewTable/FilterQuery/SET_ORDERTYPE',
  'Login/REQUEST_SUCCESS',
];

const addSlashes = array => {
  let str = '';
  array.forEach(item => {
    str += `/${item}`;
  });
  return str;
};

const dataHandler = path => {
  if (path.indexOf('/schema') === 0) {
    const parts = path.split('/').filter(p => p !== '');
    const numParts = parts.length;
    if (numParts >= 2) {
      parts[1] = 'SCHEMA_NAME';
    }
    if (numParts >= 4) {
      if (parts[2] === 'tables') {
        parts[3] = 'TABLE_NAME';
      }
    }
    return addSlashes(['data', ...parts]);
  }
  return `/data${path}`;
};

const apiExplorerHandler = () => {
  return '/api-explorer';
};

const remoteSchemasHandler = path => {
  const parts = path.split('/').filter(p => p !== '');
  const numParts = parts.length;
  if (numParts > 2) {
    parts[1] = 'REMOTE_SCHEMA_NAME';
  }
  return addSlashes(['remote-schemas', ...parts]);
};

const eventsHandler = path => {
  const parts = path.split('/').filter(p => p !== '');
  const numParts = parts.length;
  if (numParts > 3) {
    parts[2] = 'TRIGGER_NAME';
  }
  return addSlashes(['events', ...parts]);
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
