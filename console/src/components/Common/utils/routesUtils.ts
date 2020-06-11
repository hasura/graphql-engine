// import globals from '../../../Globals';

/*** DATA ROUTES ***/

export const getSchemaBaseRoute = (schemaName: string) => {
  // return `${globals.urlPrefix}/data/schema/${schemaName}`;
  return `/data/schema/${encodeURIComponent(schemaName)}`;
};

export const getSchemaAddTableRoute = (schemaName: string) => {
  return `${getSchemaBaseRoute(schemaName)}/table/add`;
};

export const getSchemaPermissionsRoute = (schemaName: string) => {
  return `${getSchemaBaseRoute(schemaName)}/permissions`;
};

const getTableBaseRoute = (
  schemaName: string,
  tableName: string,
  isTable: boolean
) => {
  return `${getSchemaBaseRoute(schemaName)}/${
    isTable ? 'tables' : 'views'
  }/${encodeURIComponent(tableName)}`;
};

export const getTableBrowseRoute = (
  schemaName: string,
  tableName: string,
  isTable: boolean
) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/browse`;
};

export const getTableInsertRowRoute = (
  schemaName: string,
  tableName: string,
  isTable: boolean
) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/insert`;
};

export const getTableEditRowRoute = (
  schemaName: string,
  tableName: string,
  isTable: boolean
) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/edit`;
};

export const getTableModifyRoute = (
  schemaName: string,
  tableName: string,
  isTable: boolean
) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/modify`;
};

export const getTableRelationshipsRoute = (
  schemaName: string,
  tableName: string,
  isTable: boolean
) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/relationships`;
};

export const getTablePermissionsRoute = (
  schemaName: string,
  tableName: string,
  isTable: boolean
) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/permissions`;
};

export const getFunctionBaseRoute = (
  schemaName: string,
  functionName: string
) => {
  return `${getSchemaBaseRoute(schemaName)}/functions/${encodeURIComponent(
    functionName
  )}`;
};

export const getFunctionModifyRoute = (
  schemaName: string,
  functionName: string
) => {
  return `${getFunctionBaseRoute(schemaName, functionName)}/modify`;
};

export const getFunctionPermissionsRoute = (
  schemaName: string,
  functionName: string
) => {
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
export const adhocEventsPrefix = 'one-off-scheduled-events';
export const dataEventsPrefix = 'data';
export type RouteType = 'absolute' | 'relative';

export const getSTRoute = (
  type: RouteType = 'absolute',
  relativeRoute: string
) => {
  if (type === 'relative') {
    return `${relativeRoute}`;
  }
  return `/${eventsPrefix}/${scheduledEventsPrefix}/${relativeRoute}`;
};
export const getETRoute = (
  type: RouteType = 'absolute',
  relativeRoute: string
) => {
  if (type === 'relative') {
    return `${relativeRoute}`;
  }
  return `/${eventsPrefix}/${dataEventsPrefix}/${relativeRoute}`;
};
export const getAdhocEventsRoute = (
  type: RouteType = 'absolute',
  relativeRoute = ''
) => {
  if (type === 'relative') {
    return `${relativeRoute}`;
  }
  return `/${eventsPrefix}/${adhocEventsPrefix}/${relativeRoute}`;
};

export const isDataEventsRoute = (route: string) => {
  return route.includes(`/${eventsPrefix}/${dataEventsPrefix}`);
};
export const isScheduledEventsRoute = (route: string) => {
  return route.includes(`/${eventsPrefix}/${scheduledEventsPrefix}`);
};
export const isAdhocScheduledEventRoute = (route: string) => {
  return route.includes(`/${eventsPrefix}/${adhocEventsPrefix}`);
};

export const getAddSTRoute = (type?: RouteType) => {
  return getSTRoute(type, 'add');
};
export const getScheduledEventsLandingRoute = (type?: RouteType) => {
  return getSTRoute(type, 'manage');
};
export const getSTModifyRoute = (stName: string, type?: RouteType) => {
  return getSTRoute(type, `${stName}/modify`);
};
export const getSTPendingEventsRoute = (stName: string, type?: RouteType) => {
  return getSTRoute(type, `${stName}/pending`);
};
export const getSTProcessedEventsRoute = (stName: string, type?: RouteType) => {
  return getSTRoute(type, `${stName}/processed`);
};
export const getSTInvocationLogsRoute = (stName: string, type?: RouteType) => {
  return getSTRoute(type, `${stName}/logs`);
};
export const getAddETRoute = (type?: any) => {
  return getETRoute(type, 'add');
};

export const getDataEventsLandingRoute = (type?: RouteType) => {
  return getETRoute(type, 'manage');
};
export const getETModifyRoute = (etName: string, type?: RouteType) => {
  return getETRoute(type, `${etName}/modify`);
};
export const getETPendingEventsRoute = (etName: string, type?: RouteType) => {
  return getETRoute(type, `${etName}/pending`);
};
export const getETProcessedEventsRoute = (etName: string, type?: RouteType) => {
  return getETRoute(type, `${etName}/processed`);
};
export const getETInvocationLogsRoute = (etName: string, type?: RouteType) => {
  return getETRoute(type, `${etName}/logs`);
};

export const getAddAdhocEventRoute = (type: RouteType) => {
  return getAdhocEventsRoute(type, 'add');
};
export const getAdhocEventsLogsRoute = (type: RouteType) => {
  return getAdhocEventsRoute(type, 'logs');
};
export const getAdhocPendingEventsRoute = (type: RouteType) => {
  return getAdhocEventsRoute(type, 'pending');
};
export const getAdhocProcessedEventsRoute = (type: RouteType) => {
  return getAdhocEventsRoute(type, 'processed');
};
export const getAdhocEventsInfoRoute = (type: RouteType) => {
  return getAdhocEventsRoute(type, 'info');
};
