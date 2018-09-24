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
  if (data.constructor.name === 'Object') {
    return 'json';
  }
  throwError(`Message: invalid data type given for column ${column}: ${typeof data}`);
};

const isForeign = (name, db, isFirebase) => {
  const idPos = name.indexOf('___id');
  if (idPos <= 0) {
    return false;
  } else {
    if (Object.keys(db).find((tableName) => tableName === name.substring(0, idPos))) {
      return true;
    }
  }
  return false;
};

const getColumnData = (dataArray, db, isFirebase) => {
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
    columnMetadata.isForeign = isForeign(column, db, isFirebase);
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

const generate = (db, isFirebase) => {
  const metaData = [];
  console.log(db);
  Object.keys(db).forEach(rootField => {
    const tableMetadata = {};
    if (!isFirebase && !hasPrimaryKey(db[rootField], rootField)) {
      throwError(`Message: a unique column with name "id" and type integer must present in table "${rootField}"`);
    }
    tableMetadata.name = rootField;
    console.log(rootField);
    tableMetadata.columns = getColumnData(db[rootField], db, isFirebase);
    tableMetadata.dependencies = [];
    tableMetadata.columns.forEach(column => {
      if (column.isForeign) {
        tableMetadata.dependencies.push(
          column.name.substring(0, isFirebase ? column.name.indexOf('___id') : column.name.length - 3)
        );
      }
    });
    metaData.push(tableMetadata);
  });
  return metaData;
};

module.exports = generate;
