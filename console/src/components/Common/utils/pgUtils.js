import React from 'react';
import { isEqual } from './jsUtils';

/*** Table/View utils ***/

// TODO: figure out better pattern for overloading fns

export const getTableName = table => {
  return table.table_name;
};

export const getTableSchema = table => {
  return table.table_schema;
};

// tableName and tableNameWithSchema are either/or arguments
export const generateTableDef = (
  tableName,
  tableSchema = 'public',
  tableNameWithSchema = null
) => {
  if (tableNameWithSchema) {
    tableSchema = tableNameWithSchema.split('.')[0];
    tableName = tableNameWithSchema.split('.')[1];
  }

  return {
    schema: tableSchema,
    name: tableName,
  };
};

export const getTableDef = table => {
  return generateTableDef(getTableName(table), getTableSchema(table));
};

// table and tableDef are either/or arguments
export const getTableNameWithSchema = (
  table,
  wrapDoubleQuotes = true,
  tableDef = null
) => {
  let _fullTableName;

  tableDef = tableDef || getTableDef(table);

  if (wrapDoubleQuotes) {
    _fullTableName =
      '"' + tableDef.schema + '"' + '.' + '"' + tableDef.name + '"';
  } else {
    _fullTableName = tableDef.schema + '.' + tableDef.name;
  }

  return _fullTableName;
};

export const checkIfTable = table => {
  return table.table_type === 'BASE TABLE';
};

export const displayTableName = table => {
  const tableName = getTableName(table);
  const isTable = checkIfTable(table);

  return isTable ? <span>{tableName}</span> : <i>{tableName}</i>;
};

export const findTable = (allTables, tableDef) => {
  return allTables.find(t => isEqual(getTableDef(t), tableDef));
};

export const getSchemaTables = (allTables, tableSchema) => {
  return allTables.filter(t => getTableSchema(t) === tableSchema);
};

export const getTrackedTables = tables => {
  return tables.filter(t => t.is_table_tracked);
};

/*** Table/View permissions utils ***/
export const getTablePermissions = (table, role = null, action = null) => {
  let tablePermissions = table.permissions;

  if (role) {
    tablePermissions = tablePermissions.find(p => p.role_name === role);

    if (tablePermissions && action) {
      tablePermissions = tablePermissions.permissions[action];
    }
  }

  return tablePermissions;
};

/*** Function utils ***/

export const getFunctionSchema = pgFunction => {
  return pgFunction.function_schema;
};

export const getFunctionName = pgFunction => {
  return pgFunction.function_name;
};
