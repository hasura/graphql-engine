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

const eventsPrefix = 'events';
const scheduledEventsPrefix = 'scheduled';
const dataEventsPrefix = 'data';
export const getAddSTRoute = () => {
  return `/${eventsPrefix}/${scheduledEventsPrefix}/add`;
};
export const getAddETRoute = () => {
  return `/${eventsPrefix}/${dataEventsPrefix}/add`;
};
export const isDataEventsRoute = route => {
  return route.includes(`/${eventsPrefix}/${dataEventsPrefix}`);
};
export const isScheduledEventsRoute = route => {
  return route.includes(`/${eventsPrefix}/${scheduledEventsPrefix}`);
};
export const getDataEventsLandingRoute = () => {
  return `/${eventsPrefix}/${dataEventsPrefix}/manage`;
};
export const getScheduledEventsLandingRoute = () => {
  return `/${eventsPrefix}/${scheduledEventsPrefix}/manage`;
};
export const getSTModifyRoute = stName => {
  return `/${eventsPrefix}/${scheduledEventsPrefix}/${stName}/modify`;
};
export const getETModifyRoute = etName => {
  return `/${eventsPrefix}/${dataEventsPrefix}/${etName}/modify`;
};
