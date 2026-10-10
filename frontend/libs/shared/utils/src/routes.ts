import {
  DataQueryType,
  QualifiedDataSource,
  Table,
  TableFunction,
} from '@hasura/shared/types';
import { To } from 'react-router';

export const getReactHelmetTitle = (feature: string, service: string) => {
  return `${feature} - ${service} | Hasura`;
};

export const getSchemaBaseRoute = (
  schemaName: string,
  sourceName = 'default',
) =>
  `/data/${encodeURIComponent(sourceName)}/schema/${encodeURIComponent(
    schemaName,
  )}`;

export const getDataSourceBaseRoute = (dataSource: string) =>
  `/data/${encodeURIComponent(dataSource)}`;

export const getSchemaPermissionsRoute = (
  schemaName: string,
  dataSource: string,
) => {
  return `${getSchemaBaseRoute(schemaName, dataSource)}/permissions`;
};

export const manageDatabasesRoute = '/data/manage';

const getTableBaseRoute = (
  schemaName: string,
  sourceName: string,
  tableName: string,
  isTable: boolean,
) =>
  `${getSchemaBaseRoute(schemaName, sourceName)}/${
    isTable ? 'tables' : 'views'
  }/${encodeURIComponent(tableName)}`;

export const getTableBrowseRoute = (
  schemaName: string,
  sourceName: string,
  tableName: string,
  isTable: boolean,
) => {
  return `${getTableBaseRoute(
    schemaName,
    sourceName,
    tableName,
    isTable,
  )}/browse`;
};

export const getTableInsertRowRoute = (
  schemaName: string,
  sourceName: string,
  tableName: string,
  isTable: boolean,
) => {
  return `${getTableBaseRoute(
    schemaName,
    sourceName,
    tableName,
    isTable,
  )}/insert`;
};

export const getTableEditRowRoute = (
  schemaName: string,
  source: string,
  tableName: string,
  isTable: boolean,
) => {
  return `${getTableBaseRoute(schemaName, source, tableName, isTable)}/edit`;
};

export const getTableModifyRoute = (
  schemaName: string,
  source: string,
  tableName: string,
  isTable: boolean,
) => {
  return `${getTableBaseRoute(schemaName, source, tableName, isTable)}/modify`;
};

export const getTableRelationshipsRoute = (
  schemaName: string,
  source: string,
  tableName: string,
  isTable: boolean,
) => {
  return `${getTableBaseRoute(
    schemaName,
    source,
    tableName,
    isTable,
  )}/relationships`;
};

export const getTablePermissionsRoute = (
  schemaName: string,
  source: string,
  tableName: string,
  isTable: boolean,
  role?: string | null,
  queryType?: DataQueryType,
): To => {
  const result = `${getTableBaseRoute(
    schemaName,
    source,
    tableName,
    isTable,
  )}/permissions`;

  if (!role || !queryType) {
    return result;
  }

  return {
    pathname: result,
    search: `?role=${role}&permission_type=${queryType}`,
  };
};

export const isTableRoute = (route: string) => {
  // contains "/data/<source-name>/schema/<schema-name>/tables\views/<table-name>/"
  return /\/data\/(.*)\/schema\/(.*)\/(tables|views)\/(.*)\//.test(route);
};

// Action route utils
export const manageActions = '/actions/manage';
export const manageAction = (name: string, operation?: string) =>
  `${manageActions}/${name}${operation ? '/' + operation : ''}`;
export const actionTypes = (operation?: string) =>
  `/actions/types${operation ? '/' + operation : ''}`;
export const createAction = `${manageActions}/add`;

// Events route utils

export const eventsPrefix = 'events';
export const scheduledEventsPrefix = 'cron';
export const adhocEventsPrefix = 'one-off-scheduled-events';
export const dataEventsPrefix = 'data';

export const getSTRoute = (type: string | undefined, relativeRoute: string) => {
  if (type === 'relative') {
    return `${relativeRoute}`;
  }
  return `/${eventsPrefix}/${scheduledEventsPrefix}/${relativeRoute}`;
};
export const getETRoute = (type: string | undefined, relativeRoute: string) => {
  if (type === 'relative') {
    return `${relativeRoute}`;
  }
  return `/${eventsPrefix}/${dataEventsPrefix}/${relativeRoute}`;
};
export const getAdhocEventsRoute = (
  type: string | undefined,
  relativeRoute?: string,
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
export const getAddSTRoute = (type?: string) => {
  return getSTRoute(type, 'add');
};
export const getScheduledEventsLandingRoute = (type?: string) => {
  return getSTRoute(type, 'manage');
};
export const getSTModifyRoute = (stName: string, type?: string) => {
  return getSTRoute(type, `${stName}/modify`);
};
export const getSTPendingEventsRoute = (stName: string, type?: string) => {
  return getSTRoute(type, `${stName}/pending`);
};
export const getSTProcessedEventsRoute = (stName: string, type?: string) => {
  return getSTRoute(type, `${stName}/processed`);
};
export const getSTInvocationLogsRoute = (stName: string, type?: string) => {
  return getSTRoute(type, `${stName}/logs`);
};
export const getAddETRoute = (type?: string) => {
  return getETRoute(type, 'add');
};
export const getDataEventsLandingRoute = (type?: string) => {
  return getETRoute(type, 'manage');
};
export const getETModifyRoute = ({
  type,
  name,
}: {
  type?: string;
  name?: string;
}) => {
  return getETRoute(type, `${name ? `${name}/` : ''}modify`);
};
export const getETPendingEventsRoute = (type?: string) => {
  return getETRoute(type, 'pending');
};
export const getETProcessedEventsRoute = (type?: string) => {
  return getETRoute(type, 'processed');
};
export const getETInvocationLogsRoute = (type?: string) => {
  return getETRoute(type, 'logs');
};

export const getAddAdhocEventRoute = (type?: string) => {
  return getAdhocEventsRoute(type, 'add');
};

export const getAdhocEventsLogsRoute = (type?: string) => {
  return getAdhocEventsRoute(type, 'logs');
};

export const getAdhocPendingEventsRoute = (type?: string) => {
  return getAdhocEventsRoute(type, 'pending');
};

export const getAdhocProcessedEventsRoute = (type?: string) => {
  return getAdhocEventsRoute(type, 'processed');
};

export const getAdhocEventsInfoRoute = (type?: string) => {
  return getAdhocEventsRoute(type, 'info');
};

// Data routes
const dataRoutePrefix = '/data';
const dataManageDatabaseRoutePrefix = `${dataRoutePrefix}/manage/source`;
export type ManageDatabaseTab =
  'schemas' | 'tables' | 'relationships' | 'functions' | 'template_gallery';

export const manageDatabaseSource = (
  database: string,
  tab?: ManageDatabaseTab,
) =>
  `${dataManageDatabaseRoutePrefix}/${encodeURIComponent(
    database,
  )}${tab ? '?tab=' + tab : ''}`;

export const connectDatabase = (driver?: string) =>
  driver
    ? `/data/manage/database/add?driver=${encodeURIComponent(driver)}`
    : '/data/manage/connect';

export const manageDatabase = '/data/manage';

export const editDatabase = (source: QualifiedDataSource): To => ({
  pathname: `/data/manage/database/edit`,
  search: `?driver=${encodeURIComponent(source.kind)}&database=${encodeURIComponent(source.name)}`,
});

export const manageTable = (
  database: string,
  table: Table,
  operation?: string | null,
): To => ({
  pathname: `${dataManageDatabaseRoutePrefix}/${database}/table${operation ? '/' + operation : ''}`,
  search: `?table=${encodeURIComponent(JSON.stringify(table))}`,
});
export const permissionSummary = (database: string) =>
  `${manageDatabaseSource(database)}/permission-summary`;

export const manageFunction = (
  dataSourceName: string,
  fn: TableFunction,
  operation?: string,
): To => {
  return {
    pathname: `${manageDatabaseSource(dataSourceName)}/function${operation ? '/' + operation : ''}`,
    search: `?function=${encodeURIComponent(JSON.stringify(fn))}`,
  };
};

export const addTable = (database: string, schema?: string): To => {
  return {
    pathname: `${manageDatabaseSource(database!)}/table/add`,
    search: schema ? `?schema=${schema}` : undefined,
  };
};
