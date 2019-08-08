export const findTable = (allTables, tableName, tableSchema = 'public') => {
  return allTables.find(
    t => t.table_name === tableName && t.table_schema === tableSchema
  );
};
