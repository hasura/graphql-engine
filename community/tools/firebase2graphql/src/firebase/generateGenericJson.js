const uuid = require('uuid/v4');

const throwError = require('../error');

const handleJSONDoc = (db) => {
  const tablesMap = {};
  const guessedTablesMap = {};
  const generateNewTable = (objectRelMetadata, arrayRelMetadata) => {
    if (arrayRelMetadata) {
      const newTableName = `${arrayRelMetadata.tableName}_${arrayRelMetadata.name}`;
      const parentTableName = arrayRelMetadata.tableName;
      const pkeys = arrayRelMetadata.pkeys;
      if (!tablesMap[newTableName]) {
        tablesMap[newTableName] = [];
      }
      tablesMap[newTableName] = [
        ...tablesMap[newTableName],
        ...arrayRelMetadata.data.map((item) => {
          const newItem = {
            ...item
          };
          for (var pkey in pkeys) {
            newItem[`${parentTableName}_${pkey}`] = pkeys[pkey];
          }
          if (newItem['__id'] === undefined) {
            newItem[getLastId(newItem)] = uuid();
          }
          return newItem;
        })
      ];
    } else {
      const newTableName = objectRelMetadata.tableName + '_' +objectRelMetadata.name;
      const newItem = {
        ...objectRelMetadata.data
      };
      if (!tablesMap[newTableName]) {
        tablesMap[newTableName] = [];
      }
      if (!tablesMap[newTableName].find((row) => {
        for (var column in row) {
          if (row[column] !== newItem[column]) {
            return false;
          }
          return true;
        }
      })) {
        tablesMap[newTableName].push(newItem);
      }
    }
  }

  for (var tableName in db) {
    tablesMap[tableName] = handleTable(
      db[tableName],
      tableName,
      generateNewTable
    );
  }
 
  return tablesMap;
}

const handleTable = (obj, tableName, tableDetectedCallback) => { 
  const rowArray = [];
  const flatten = (object, row) => { 
    if (isObjectList(object)) {
      const dummyRow = { ...row }
      for (var key in object) {
        row[getIdNumber(dummyRow)] = key;
        const value = object[key];
        const newRow = { ...flatten(value, row)};
        if (newRow && Object.keys(newRow).length > 0) {
          rowArray.push(newRow);
        }
      }
    } else if (isList(object)){
      for (var key in object) {
        const dummyRow = { ...row };
        dummyRow[getIdNumber(dummyRow)] = uuid();
        dummyRow.value = key;
        if (Object.keys(dummyRow).length > 0) {
          rowArray.push(dummyRow);
        }
      }
    } else {
      for (var key in object) {
        const value = object[key];
        if (value === null || value.constructor.name !== 'Object') {
          row[key] = value;
        } else if (value.constructor.name === 'Object') {
          const pkeyMap = getPrimaryKeys(row); 
          if (isList(value)) {
            tableDetectedCallback(
              null,
              {
                tableName,
                name: key,
                pkeys: pkeyMap,
                data: Object.keys(value).map((item) => ({ __value: item}))
              }
            );
          } else if (isObjectList(value)) {
            tableDetectedCallback(
              null,
              {
                tableName,
                name: key,
                pkeys: pkeyMap,
                data: handleTable(value, `${tableName}_${key}`, tableDetectedCallback)
              }
            );
          } else {
            if (Object.keys(value).length !== 0) {
              const newUUID = uuid();
              row[`${tableName}_${key}___id`] = newUUID;
              tableDetectedCallback(
                {
                  tableName,
                  name: key,
                  data: flatten(value, {__id: newUUID})
                }
              )
            }
          }
        }
      }
      return row;
    }
  }
  if (!isObjectList(obj)) {
    if (isRandomList(obj)) {
      for (var key in obj) {
        rowArray.push({
          '__key': key,
          '__value': obj[key],
          '__id': uuid()
        });
      }
      return rowArray;
    } else {
      throwError('Message: invalid JSON provided for node ' + tableName);
    }
  }
  for (var key in obj) {
    const flatRow = flatten(obj[key], { __id: key });
    if (flatRow && Object.keys(flatRow).length > 0) { rowArray.push(flatRow); }  
  }
  return rowArray;
}
 const getPrimaryKeys = (obj) => {
  const pkeyMap = {}; 
  for (var key in obj) {
    if(key.indexOf('__id') === 0) {
      pkeyMap[key] = obj[key];
    }
  }
  return pkeyMap;
}

const getLastId = (obj, index = 0) => {
  const id = index === 0 ? '__id' : `__id_${index}`;
  const nextIndex = index === 0 ? 2 : index + 1;
  if (!obj[`__id_${nextIndex}`]) {
    return id;
  } else {
    getLastId(obj, nextIndex);
  }
}

const getIdNumber = (obj, index = 0) => {
  const id = index === 0 ? '__id' : `__id_${index}`;
  const nextIndex = index === 0 ? 2 : index + 1;
  if (obj[id] === undefined) {
    return id;
  } else {
    return getIdNumber(obj, nextIndex);
  }
}

const isRandomList = (obj) => {
  if (!obj) {
    return false;
  }
  for (var key in obj) {
    if (obj[key] !== null && typeof obj[key] === 'object') {
      return false;
    }
  }
  return true;
}

const isList = (obj) => {
  if (Object.keys(obj).length === 0) {
    return false;
  }
  for (var key in obj) {
    if (obj[key] === null) {
      return false;
    }
    if ( obj[key].constructor.name !== 'Boolean' || !obj[key]) {
      return false;
    }
  }
  return true;
}

const isObjectList = (obj) => {
  if (obj === null || obj === undefined) {
    return false;
  }
  const listChildStructure = {}
  for (var key in obj) {
    if (obj[key] === null) {
      return false;
    }
    if (typeof obj[key] !== 'object') {
      return false;
    }
    if (Object.keys(obj[key]).length === 0) {
      return false;
    }

    for (var childKey in obj[key]) {
      if (!listChildStructure[childKey]) {
        if(obj[key][childKey] !== null && obj[key][childKey] !== undefined) {
          listChildStructure[childKey] = typeof obj[key][childKey];
        }
      } else {
        if (obj[key][childKey] !== null && obj[key][childKey] !== undefined) {
          if (typeof obj[key][childKey] !== listChildStructure[childKey]) {
            return false;
          }
        }
      }
    }
  }
  return true;
};

module.exports = handleJSONDoc;

