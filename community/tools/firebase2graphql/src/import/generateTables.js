const throwError = require('../error');
const validateUUID = require('uuid-validate');

const getDataType = (data, column) => {
  if (typeof data === 'number') {
    return (data === parseInt(data, 10)) ? 'bigint' : 'numeric';
  }
  if (typeof data === 'string' || data === null) {
    if (data && validateUUID(data)) {
      return 'uuid';
    }
    return 'text';
  }
  if (typeof data === 'boolean') {
    return 'boolean';
  }
  if (data.constructor.name === 'Date') {
    return 'timestamptz';
  }
  if (data.constructor.name === 'Object' || data.constructor.name === 'Array') {
    return 'json';
  }
  throwError(`Message: invalid data type given for column ${column}: ${data.constructor.name}`);
};

const isForeign = (name, db) => {
  const idPos = name.indexOf('__id');
  if (idPos <= 0) {
    return false;
  }
  if (Object.keys(db).find(tableName => tableName === name.substring(0, idPos))) {
    return true;
  }

  return false;
};

const getColumnData = (dataArray, db) => {
  if (dataArray.length === 0) {
    return [];
  }
  const refRow = {
    numOfCols: 0,
    index: 0,
  };
  dataArray.forEach((row, i) => {
    if (Object.keys(row).length > refRow.numOfCols) {
      refRow.numOfCols = Object.keys(row).length;
      refRow.index = i;
    }
  });
  const refColumns = dataArray[refRow.index];
  const columnData = [];
  Object.keys(refColumns).forEach(column => {
    const columnMetadata = {};
    if (!column) {
      throwError("Message: column names can't be empty strings");
    }
    columnMetadata.name = column;
    const sampleData = refColumns[column];
    columnMetadata.type = getDataType(sampleData, column, db);
    columnMetadata.isForeign = isForeign(column, db);
    columnData.push(columnMetadata);
  });
  return columnData;
};

const generate = db => {
  const metaData = [];
  Object.keys(db).forEach(rootField => {
    if (db[rootField].length === 0) {
      return;
    }
    const tableMetadata = {};
    tableMetadata.name = rootField;
    tableMetadata.columns = getColumnData(db[rootField], db);
    tableMetadata.dependencies = [];
    tableMetadata.columns.forEach(column => {
      if (column.isForeign) {
        tableMetadata.dependencies.push(
          column.name.substring(0, column.name.indexOf('__id'))
        );
      }
    });
    metaData.push(tableMetadata);
  });
  return metaData;
};

module.exports = generate;
