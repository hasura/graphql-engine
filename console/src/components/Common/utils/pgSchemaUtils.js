export const getTableName = table => {
  return table.table_name;
};

export const getTableSchema = table => {
  return table.table_schema;
};

export const checkIfTable = table => {
  return table.table_type === 'BASE TABLE';
};

export const findTable = (allTables, tableName, tableSchema = 'public') => {
  return allTables.find(
    t => getTableName(t) === tableName && getTableSchema(t) === tableSchema
  );
};
