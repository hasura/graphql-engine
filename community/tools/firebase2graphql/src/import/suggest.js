const {log} = require('../log');
const colors = require('colors/safe');

const isSubset = (array1, array2) => {
  return array2.every(item => array1.includes(item));
};

const getTableColumns = obj => {
  const columns = {};
  for (var key in obj) {
    if (key.indexOf('_id') === -1) {
      columns[key] = [];
    }
  }
  return columns;
};

const getColumnsMap = db => {
  const columnMap = {};
  for (var tableName in db) {
    columnMap[tableName] = getTableColumns(db[tableName][0]);
    db[tableName].forEach(row => {
      for (var key in columnMap[tableName]) {
        columnMap[tableName][key].push(row[key]);
      }
    });
  }
  return columnMap;
};

const getDuplicates = db => {
  const tableColumnMap = getColumnsMap(db);
  const maybeDuplicates = {};
  for (var t1 in tableColumnMap) {
    if (!maybeDuplicates[t1]) {
      maybeDuplicates[t1] = [];
    }
    for (var t2 in tableColumnMap) {
      if (!maybeDuplicates[t1]) {
        maybeDuplicates[t2] = [];
      }
      if (t1 !== t2) {
        for (var key in tableColumnMap[t1]) {
          if (tableColumnMap[t2][key]) {
            if (isSubset(tableColumnMap[t1][key], tableColumnMap[t2][key])) {
              maybeDuplicates[t1].push(t2);
              break;
            }
          }
        }
      }
    }
  }
  return maybeDuplicates;
};

const suggest = (db, url) => {
  const maybeDuplicates = (getDuplicates(db));
  const newDuplicates = {
    ...maybeDuplicates,
  };

  let count = 1;
  const dupes = [];
  for (var tableName in newDuplicates) {
    maybeDuplicates[tableName].forEach(dup => {
      dupes.push(`${count++}. ${colors.yellow(tableName)} could be same as ${colors.yellow(dup)}`);
    });
  }
  if (dupes.length > 0) {
    log('');
    log('Warning:', 'yellow');
    log('While importing your data, the following duplicate tables might have been created:', 'yellow');
    dupes.forEach(dupe => log(dupe));
    log(`You can either re-run the command with the flag "--normalize", or normalize your database yourself at ${url}/console/data/schema/public`, 'yellow');
  }
};

module.exports = suggest;
