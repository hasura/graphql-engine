import React from 'react';
import { isEqual } from './jsUtils';

/*** Table/View utils ***/

export const getTableName = table => {
  return table.table_name;
};

export const getTableSchema = table => {
  return table.table_schema;
};

export const generateTableDef = (tableName, tableSchema = 'public') => {
  return {
    schema: tableSchema,
    name: tableName,
  };
};

export const getTableDef = table => {
  return generateTableDef(getTableName(table), getTableSchema(table));
};

// TODO: figure out better pattern for overloading fns
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

/*** Function utils ***/

export const getFunctionSchema = pgFunction => {
  return pgFunction.function_schema;
};

export const getFunctionName = pgFunction => {
  return pgFunction.function_name;
};
