import { ThunkDispatch } from 'redux-thunk';
import { push } from 'react-router-redux';
import { AnyAction } from 'redux';

import globals from '../../../Globals';
import { baseUrl } from '../../../Endpoints';
import { ReduxState } from '../../../types';

export const getSchemaBaseRoute = (
  schemaName: string,
  sourceName = 'default'
) =>
  `/data/${encodeURIComponent(sourceName)}/schema/${encodeURIComponent(
    schemaName
  )}`;

export const getDataSourceBaseRoute = (dataSource: string) =>
  `/data/${encodeURIComponent(dataSource)}`;

export const getSchemaAddTableRoute = (
  schemaName: string,
  sourceName: string
) => {
  return `${getSchemaBaseRoute(schemaName, sourceName)}/table/add`;
};

export const getSchemaManageRoute = () => {
  return `${baseUrl}/data/manage`;
};

export const getSchemaPermissionsRoute = (
  schemaName: string,
  dataSource: string
) => {
  return `${getSchemaBaseRoute(schemaName, dataSource)}/permissions`;
};

export const manageDatabasesRoute = '/data/manage';

const getTableBaseRoute = (
  schemaName: string,
  sourceName: string,
  tableName: string,
  isTable: boolean
) =>
  `${getSchemaBaseRoute(schemaName, sourceName)}/${
    isTable ? 'tables' : 'views'
  }/${encodeURIComponent(tableName)}`;

export const getTableBrowseRoute = (
  schemaName: string,
  sourceName: string,
  tableName: string,
  isTable: boolean
) => {
  return `${getTableBaseRoute(
    schemaName,
    sourceName,
    tableName,
    isTable
  )}/browse`;
};

export const getTableInsertRowRoute = (
  schemaName: string,
  sourceName: string,
  tableName: string,
  isTable: boolean
) => {
  return `${getTableBaseRoute(
    schemaName,
    sourceName,
    tableName,
    isTable
  )}/insert`;
};

export const getTableEditRowRoute = (
  schemaName: string,
  source: string,
  tableName: string,
  isTable: boolean
) => {
  return `${getTableBaseRoute(schemaName, source, tableName, isTable)}/edit`;
};

export const getTableModifyRoute = (
  schemaName: string,
  source: string,
  tableName: string,
  isTable: boolean
) => {
  return `${getTableBaseRoute(schemaName, source, tableName, isTable)}/modify`;
};

export const getTableRelationshipsRoute = (
  schemaName: string,
  source: string,
  tableName: string,
  isTable: boolean
) => {
  return `${getTableBaseRoute(
    schemaName,
    source,
    tableName,
    isTable
  )}/relationships`;
};

export const getTablePermissionsRoute = (
  schemaName: string,
  source: string,
  tableName: string,
  isTable: boolean
) => {
  return `${getTableBaseRoute(
    schemaName,
    source,
    tableName,
    isTable
  )}/permissions`;
};

export const getFunctionBaseRoute = (
  schemaName: string,
  source: string,
  functionName: string
) => {
  return `${getSchemaBaseRoute(
    schemaName,
    source
  )}/functions/${encodeURIComponent(functionName)}`;
};

export const getFunctionModifyRoute = (
  schemaName: string,
  source: string,
  functionName: string
) => {
  return `${getFunctionBaseRoute(schemaName, source, functionName)}/modify`;
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
  relativeRoute?: string
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

export const redirectToMetadataStatus = () => {
  return (dispatch: ThunkDispatch<ReduxState, unknown, AnyAction>) => {
    return dispatch(
      push(`${globals.urlPrefix}/settings/metadata-status?is_redirected=true`)
    );
  };
};
