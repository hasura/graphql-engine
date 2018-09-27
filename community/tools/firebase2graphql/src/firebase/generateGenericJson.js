const uuid = require('uuid/v4');

const throwError = require('../error');

const getPrimaryKeys = obj => {
  const pkeyMap = {};
  for (var pkey in obj) {
    if (pkey.indexOf('_id') === 0) {
      pkeyMap[pkey] = obj[pkey];
    }
  }
  return pkeyMap;
};

const getLastId = (obj, index = 0, selfGenerated='') => {
  const id = index === 0 ? `_id${selfGenerated}` : `_id${selfGenerated}_${index}`;
  const nextIndex = index === 0 ? 2 : index + 1;
  if (!obj[`_id_${nextIndex}`]) {
    return id;
  }
  getLastId(obj, nextIndex, selfGenerated);
};

const getIdNumber = (obj, index = 0, selfGenerated = '') => {
  const id = index === 0 ? `_id${selfGenerated}` : `_id${selfGenerated}_${index}`;
  const nextIndex = index === 0 ? 2 : index + 1;
  if (obj[id] === undefined) {
    return id;
  }
  return getIdNumber(obj, nextIndex, selfGenerated);
};

const isRandomList = obj => {
  if (!obj) {
    return false;
  }
  for (var objKey in obj) {
    if (obj[objKey] !== null && typeof obj[objKey] === 'object') {
      return false;
    }
  }
  return true;
};

const isList = obj => {
  if (Object.keys(obj).length === 0) {
    return false;
  }
  for (var objKey in obj) {
    if (obj[objKey] === null) {
      return false;
    }
    if (obj[objKey].constructor.name !== 'Boolean' || !obj[objKey]) {
      return false;
    }
  }
  return true;
};

const isObjectList = obj => {
  if (obj === null || obj === undefined) {
    return false;
  }
  const listChildStructure = {};
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
        if (obj[key][childKey] !== null && obj[key][childKey] !== undefined) {
          listChildStructure[childKey] = typeof obj[key][childKey];
        }
      } else if (obj[key][childKey] !== null && obj[key][childKey] !== undefined) {
        if (typeof obj[key][childKey] !== listChildStructure[childKey]) {
          return false;
        }
      }
    }
  }
  return true;
};

const handleTable = (obj, tableName, tableDetectedCallback) => {
  const rowArray = [];
  const flatten = (object, row) => {
    if (isObjectList(object)) {
      const dummyRow = {...row};
      for (var objListKey in object) {
        row[getIdNumber(dummyRow)] = objListKey;
        const value = object[objListKey];
        const newRow = {...flatten(value, row)};
        if (newRow && Object.keys(newRow).length > 0) {
          rowArray.push(newRow);
        }
      }
    } else if (isList(object)) {
      for (var listKey in object) {
        const dummyRow = {...row};
        dummyRow[getIdNumber(dummyRow, null, 'self')] = uuid();
        dummyRow.value = listKey;
        if (Object.keys(dummyRow).length > 0) {
          rowArray.push(dummyRow);
        }
      }
    } else {
      for (var objectKey in object) {
        const value = object[objectKey];
        if (value === null || value.constructor.name !== 'Object') {
          row[objectKey] = value;
        } else if (value.constructor.name === 'Object') {
          const pkeyMap = getPrimaryKeys(row);
          if (isList(value)) {
            tableDetectedCallback(
              null,
              {
                tableName,
                name: objectKey,
                pkeys: pkeyMap,
                data: Object.keys(value).map(item => ({__value: item})),
              }
            );
          } else if (isObjectList(value)) {
            tableDetectedCallback(
              null,
              {
                tableName,
                name: objectKey,
                pkeys: pkeyMap,
                data: handleTable(value, `${tableName}_${objectKey}`, tableDetectedCallback),
              }
            );
          } else if (Object.keys(value).length !== 0) {
            const newUUID = uuid();
            
            tableDetectedCallback(
              {
                tableName,
                name: objectKey,
                data: flatten(value, {_idself: newUUID}),
                callback: (id) => {
                  row[`${tableName}_${objectKey}__idself`] = id ? id : newUUID;
                }
              }
            );
          }
        }
      }
      return row;
    }
  };
  if (!isObjectList(obj)) {
    if (isRandomList(obj)) {
      for (var objKey in obj) {
        rowArray.push({
          __key: objKey,
          __value: obj[objKey],
          _id: uuid(),
        });
      }
      return rowArray;
    }
    throwError('Message: invalid JSON provided for node ' + tableName);
  }
  for (var id in obj) {
    const flatRow = flatten(obj[id], {_id: id});
    if (flatRow && Object.keys(flatRow).length > 0) {
      rowArray.push(flatRow);
    }
  }
  return rowArray;
};

const handleJSONDoc = db => {
  const tablesMap = {};
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
        ...arrayRelMetadata.data.map(item => {
          const newItem = {
            ...item,
          };
          for (var pkey in pkeys) {
            newItem[`${parentTableName}_${pkey}`] = pkeys[pkey];
          }
          if (newItem._id === undefined) {
            newItem[getLastId(newItem, 0, 'self')] = uuid();
          }
          return newItem;
        }),
      ];
    } else {
      const newTableName = objectRelMetadata.tableName + '_' + objectRelMetadata.name;
      const newItem = {
        ...objectRelMetadata.data,
      };
      if (!tablesMap[newTableName]) {
        tablesMap[newTableName] = [];
      }
      if (!tablesMap[newTableName].find(row => { // eslint-disable-line array-callback-return
        for (var column in row) {
          if (column.indexOf('_id') !== 0) {
            if (row[column] !== newItem[column]) {
              return false;
            }
          }
        }
        objectRelMetadata.callback(row['_idself']);
        return true;
      })) {
        tablesMap[newTableName].push(newItem);
        if (objectRelMetadata.callback) { objectRelMetadata.callback(); }
      }
    }
  };

  for (var tableName in db) {
    tablesMap[tableName] = handleTable(
      db[tableName],
      tableName,
      generateNewTable
    );
  }

  return tablesMap;
};

module.exports = handleJSONDoc;

