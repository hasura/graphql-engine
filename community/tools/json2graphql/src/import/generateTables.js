const throwError = require('./error');

const getDataType = (data, column) => {
  if (typeof data === 'number') {
    return (data === parseInt(data, 10)) ? 'int' : 'numeric';
  }
  if (typeof data === 'string' || data === null) {
    return 'text';
  }
  if (typeof data === 'boolean') {
    return 'boolean';
  }
  if (data.constructor.name === 'Date') {
    return 'timestamptz';
  }
  if (data.constructor.name === 'Object') {
    return 'json';
  }
  throwError(`message: invalid data type given for column ${column}: ${typeof data}`);
};

const isForeign = (name, db) => {
  const l = name.length;
  if (l > 3) {
    if (name.substring(l - 3, l) === '_id' &&
        Object.keys(db).find(tableName => {
          return tableName === name.substring(0, l - 3);
        })) {
      return true;
    }
  }
  return false;
};

const getColumnData = (dataArray, db) => {
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
      throwError("message: column names can't be empty strings");
    }
    columnMetadata.name = column;
    const sampleData = refColumns[column];
    columnMetadata.type = getDataType(sampleData, column, db);
    columnMetadata.isForeign = isForeign(column, db);
    columnData.push(columnMetadata);
  });
  return columnData;
};

const hasPrimaryKey = dataObj => {
  let has = true;
  dataObj.forEach(obj => {
    if (!Object.keys(obj).find(name => name === 'id')) {
      has = false;
    }
  });
  return has;
};

const generate = db => {
  const metaData = [];
  Object.keys(db).forEach(rootField => {
    const tableMetadata = {};
    if (!hasPrimaryKey(db[rootField], rootField)) {
      throwError(`message: a unique column with name "id" and type integer must present in table "${rootField}"`);
    }
    tableMetadata.name = rootField;
    tableMetadata.columns = getColumnData(db[rootField], db);
    tableMetadata.dependencies = [];
    tableMetadata.columns.forEach(column => {
      if (column.isForeign) {
        tableMetadata.dependencies.push(column.name.substring(0, column.name.length - 3));
      }
    });
    metaData.push(tableMetadata);
  });
  return metaData;
};

module.exports = generate;
