import globals from '../../../Globals';
import { getTableSchema, getTableName, checkIfTable } from './pgSchemaUtils';

export const getSchemaRoute = schemaName => {
  return `${globals.urlPrefix}/data/schema/${schemaName}`;
};

export const getTableBaseRoute = table => {
  return `${getSchemaRoute(getTableSchema(table))}/${
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
