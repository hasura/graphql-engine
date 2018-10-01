const uuid = require('uuid/v4');
const {
  getParentPrimaryKeyMap,
  getLastPrimaryKey,
  getPrimaryKeyName,
  isRandomList,
  isList,
  isObjectList
} = require('./utils');
const throwError = require('../error');

const handleTableCandidate = (obj, tableName, tableDetectedCallback) => {
  const rowArray = [];
  const flattenObject = (object, row, parent) => {
    if (isObjectList(object)) {
      const dummyRow = {...row};
      for (var objListKey in object) {
        row[getPrimaryKeyName(dummyRow)] = objListKey;
        const newRow = {...flattenObject(object[objListKey], row)};
        if (newRow && Object.keys(newRow).length > 0) {
          rowArray.push(newRow);
        }
      }
    } else if (isList(object)) {
      for (var listKey in object) {
        const dummyRow = {...row};
        dummyRow[getPrimaryKeyName(dummyRow, null, 'self')] = uuid();
        dummyRow.__value = listKey;
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
          const pkeyMap = getParentPrimaryKeyMap(row);
          if (isList(value)) {
            tableDetectedCallback(
              null,
              {
                tableName: parent || tableName,
                name: objectKey,
                pkeys: pkeyMap,
                data: Object.keys(value).map(item => ({__value: item})),
              }
            );
          } else if (isObjectList(value)) {
            tableDetectedCallback(
              null,
              {
                tableName: parent || tableName,
                name: objectKey,
                pkeys: pkeyMap,
                data: handleTableCandidate(value, `${tableName}_${objectKey}`, tableDetectedCallback),
              }
            );
          } else if (Object.keys(value).length !== 0) {
            const newUUID = uuid();
            row[`${tableName}_${objectKey}__idself`] = newUUID;
            tableDetectedCallback(
              {
                tableName,
                name: objectKey,
                data: flattenObject(value, {_idself: newUUID}, `${tableName}_${objectKey}`),
              }
            );
          }
        }
      }
      return row;
    }
  };
  if (!isObjectList(obj)) {
    if (isList(obj)) {
      for (var listKey in obj) {
        rowArray.push({
          __value: listKey,
          _id: uuid(),
        });
      }
      return rowArray;
    }
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
    const flatRow = flattenObject(obj[id], {_id: id});
    if (flatRow && Object.keys(flatRow).length > 0) {
      rowArray.push(flatRow);
    }
  }
  return rowArray;
};

const handleFirebaseJson = db => {
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
          if (newItem._idself === undefined && newItem._id === undefined) {
            newItem[getLastPrimaryKey(newItem, 0, 'self')] = uuid();
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
      tablesMap[newTableName].push(newItem);
    }
  };

  for (var tableName in db) {
    tablesMap[tableName] = handleTableCandidate(
      db[tableName],
      tableName,
      generateNewTable
    );
  }

  return tablesMap;
};

module.exports = handleFirebaseJson;

