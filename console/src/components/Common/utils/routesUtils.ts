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

const getTableBaseRoute = (schemaName: string, tableName: string, isTable: boolean) => {
  return `${getSchemaBaseRoute(schemaName)}/${
    isTable ? 'tables' : 'views'
  }/${encodeURIComponent(tableName)}`;
};

export const getTableBrowseRoute = (schemaName: string, tableName: string, isTable: boolean) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/browse`;
};

export const getTableInsertRowRoute = (schemaName: string, tableName: string, isTable: boolean) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/insert`;
};

export const getTableEditRowRoute = (schemaName: string, tableName: string, isTable: boolean) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/edit`;
};

export const getTableModifyRoute = (schemaName: string, tableName: string, isTable: boolean) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/modify`;
};

export const getTableRelationshipsRoute = (schemaName: string, tableName: string, isTable: boolean) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/relationships`;
};

export const getTablePermissionsRoute = (schemaName: string, tableName: string, isTable: boolean) => {
  return `${getTableBaseRoute(schemaName, tableName, isTable)}/permissions`;
};

export const getFunctionBaseRoute = (schemaName: string, functionName: string) => {
  return `${getSchemaBaseRoute(schemaName)}/functions/${encodeURIComponent(
    functionName
  )}`;
};

export const getFunctionModifyRoute = (schemaName: string, functionName: string) => {
  return `${getFunctionBaseRoute(schemaName, functionName)}/modify`;
};

export const getFunctionPermissionsRoute = (schemaName: string, functionName: string) => {
  return `${getFunctionBaseRoute(schemaName, functionName)}/permissions`;
};

// Action route utils

export const getActionsBaseRoute = () => {
  return '/actions/manage';
};

export const getActionsCreateRoute = () => {
  return `${getActionsBaseRoute()}/add`;
};
