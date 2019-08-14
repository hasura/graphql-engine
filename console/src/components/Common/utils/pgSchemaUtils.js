import React from 'react';

/*** Table/View utils ***/

export const getTableName = table => {
  return table.table_name;
};

export const getTableSchema = table => {
  return table.table_schema;
};

export const checkIfTable = table => {
  return table.table_type === 'BASE TABLE';
};

export const displayTableName = table => {
  const tableName = getTableName(table);
  const isTable = checkIfTable(table);

  return isTable ? <span>{tableName}</span> : <i>{tableName}</i>;
};

export const findTable = (allTables, tableName, tableSchema = 'public') => {
  return allTables.find(
    t => getTableName(t) === tableName && getTableSchema(t) === tableSchema
  );
};

/*** Function utils ***/

export const getFunctionSchema = pgFunction => {
  return pgFunction.function_schema;
};

export const getFunctionName = pgFunction => {
  return pgFunction.function_name;
};
