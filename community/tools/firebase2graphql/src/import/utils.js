const refineJson = db => {
  const newDb = {};
  for (var tableName in db) {
    const newTableName = tableName.replace(/[^a-zA-Z0-9]/g, '_');
    newDb[newTableName] = [];
    db[tableName].forEach(row => {
      const newRow = {};
      for (var colName in row) {
        newRow[colName.replace(/[^a-zA-Z0-9]/g, '_')] = row[colName];
      }
      newDb[newTableName].push(newRow);
    });
  }
  return newDb;
};

module.exports = {
  refineJson,
};
