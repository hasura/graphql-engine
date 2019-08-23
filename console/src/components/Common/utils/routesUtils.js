// import globals from '../../../Globals';
import {
  getTableSchema,
  getTableName,
  checkIfTable,
  getFunctionSchema,
  getFunctionName,
} from './pgUtils';

/*** DATA ROUTES ***/

export const getSchemaBaseRoute = schemaName => {
  // return `${globals.urlPrefix}/data/schema/${schemaName}`;
  return `/data/schema/${schemaName}`;
};

export const getSchemaAddTableRoute = schemaName => {
  return `${getSchemaBaseRoute(schemaName)}/table/add`;
};

export const getSchemaPermissionsRoute = schemaName => {
  return `${getSchemaBaseRoute(schemaName)}/permissions`;
};

export const getTableBaseRoute = table => {
  return `${getSchemaBaseRoute(getTableSchema(table))}/${
    checkIfTable(table) ? 'tables' : 'views'
  }/${getTableName(table)}`;
};

export const getTableBrowseRoute = table => {
  return `${getTableBaseRoute(table)}/browse`;
};

export const getTableModifyRoute = table => {
  return `${getTableBaseRoute(table)}/modify`;
};

export const getTableRelationshipsRoute = table => {
  return `${getTableBaseRoute(table)}/relationships`;
};

export const getTablePermissionsRoute = table => {
  return `${getTableBaseRoute(table)}/permissions`;
};

export const getFunctionBaseRoute = pgFunction => {
  return `${getSchemaBaseRoute(
    getFunctionSchema(pgFunction)
  )}/functions/${getFunctionName(pgFunction)}`;
};

export const getFunctionModifyRoute = pgFunction => {
  return `${getFunctionBaseRoute(pgFunction)}/modify`;
};

export const getFunctionPermissionsRoute = pgFunction => {
  return `${getFunctionBaseRoute(pgFunction)}/permissions`;
};
