// import globals from '../../../Globals';

/*** DATA ROUTES ***/

export const getSchemaBaseRoute = schemaName => {
  // return `${globals.urlPrefix}/data/schema/${schemaName}`;
  return `/data/schema/${encodeURIComponent(schemaName)}`;
};

export const getSchemaAddTableRoute = schemaName => {
  return `${getSchemaBaseRoute(schemaName)}/table/add`;
};

export const getSchemaPermissionsRoute = schemaName => {
  return `${getSchemaBaseRoute(schemaName)}/permissions`;
};

const getTableBaseRoute = (schemaName, tableName, isTable) => {
  return `${getSchemaBaseRoute(schemaName)}/${
    isTable ? 'tables' : 'views'
  }/${encodeURIComponent(tableName)}`;
};

export const getTableBrowseRoute = (schemaName, tableName, isTable) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/browse`;
};

export const getTableInsertRowRoute = (schemaName, tableName, isTable) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/insert`;
};

export const getTableEditRowRoute = (schemaName, tableName, isTable) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/edit`;
};

export const getTableModifyRoute = (schemaName, tableName, isTable) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/modify`;
};

export const getTableRelationshipsRoute = (schemaName, tableName, isTable) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/relationships`;
};

export const getTablePermissionsRoute = (schemaName, tableName, isTable) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/permissions`;
};

export const getFunctionBaseRoute = (schemaName, functionName) => {
  return `${getSchemaBaseRoute(schemaName)}/functions/${encodeURIComponent(
    functionName
  )}`;
};

export const getFunctionModifyRoute = (schemaName, functionName) => {
  return `${getFunctionBaseRoute(schemaName, functionName)}/modify`;
};

export const getFunctionPermissionsRoute = (schemaName, functionName) => {
  return `${getFunctionBaseRoute(schemaName, functionName)}/permissions`;
};

// Action route utils

export const getActionsBaseRoute = () => {
  return '/actions/manage';
};

export const getActionsCreateRoute = () => {
  return `${getActionsBaseRoute()}/add`;
};

// Events route utils

export const eventsPrefix = 'events';
export const scheduledEventsPrefix = 'cron';
export const adhocEventsPrefix = 'independent-scheduled-events';
export const dataEventsPrefix = 'data';
export const routeType = 'absolute' | 'relative';

export const getSTRoute = (type, relativeRoute) => {
  if (type === 'relative') {
    return `${relativeRoute}`;
  }
  return `/${eventsPrefix}/${scheduledEventsPrefix}/${relativeRoute}`;
};
export const getETRoute = (type, relativeRoute) => {
  if (type === 'relative') {
    return `${relativeRoute}`;
  }
  return `/${eventsPrefix}/${dataEventsPrefix}/${relativeRoute}`;
};
export const getAdhocEventsRoute = (type, relativeRoute) => {
  if (type === 'relative') {
    return `${relativeRoute}`;
  }
  return `/${eventsPrefix}/${adhocEventsPrefix}/${relativeRoute}`;
};

export const isDataEventsRoute = route => {
  return route.includes(`/${eventsPrefix}/${dataEventsPrefix}`);
};
export const isScheduledEventsRoute = route => {
  return route.includes(`/${eventsPrefix}/${scheduledEventsPrefix}`);
};
export const isAdhocScheduledEventRoute = route => {
  return route.includes(`/${eventsPrefix}/${adhocEventsPrefix}`);
};
export const getAddSTRoute = type => {
  return getSTRoute(type, 'add');
};
export const getScheduledEventsLandingRoute = type => {
  return getSTRoute(type, 'manage');
};
export const getSTModifyRoute = (stName, type) => {
  return getSTRoute(type, `${stName}/modify`);
};
export const getSTPendingEventsRoute = (stName, type) => {
  return getSTRoute(type, `${stName}/pending`);
};
export const getSTProcessedEventsRoute = (stName, type) => {
  return getSTRoute(type, `${stName}/processed`);
};
export const getSTInvocationLogsRoute = (stName, type) => {
  return getSTRoute(type, `${stName}/logs`);
};
export const getAddETRoute = type => {
  return getETRoute(type, 'add');
};
export const getDataEventsLandingRoute = type => {
  return getETRoute(type, 'manage');
};
export const getETModifyRoute = (etName, type) => {
  return getETRoute(type, `${etName}/modify`);
};
export const getETPendingEventsRoute = (etName, type) => {
  return getETRoute(type, `${etName}/pending`);
};
export const getETProcessedEventsRoute = (etName, type) => {
  return getETRoute(type, `${etName}/processed`);
};
export const getETInvocationLogsRoute = (etName, type) => {
  return getETRoute(type, `${etName}/logs`);
};
export const getAddAdhocEventRoute = type => {
  return getAdhocEventsRoute(type, 'add');
};
export const getAdhocEventsLogsRoute = type => {
  return getAdhocEventsRoute(type, 'logs');
};
export const getAdhocPendingEventsRoute = type => {
  return getAdhocEventsRoute(type, 'pending');
};
export const getAdhocProcessedEventsRoute = type => {
  return getAdhocEventsRoute(type, 'processed');
};
