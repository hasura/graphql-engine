const uuid = require('uuid/v4');
const {
  getParentPrimaryKeyMap,
  getLastPrimaryKey,
  getPrimaryKeyName,
  isRandomList,
  isList,
  isObjectList,
  makeFirebaseListFromObj,
  makeFirebaseListFromArr,
} = require('./utils');
const throwError = require('../error');

const handleTableCandidate = (obj, tableName, tableDetectedCallback, isRootLevel) => {
  const rowArray = [];
  const flattenObject = (object, row, parent) => {
    if (isObjectList(object)) {
      const firebaseList = makeFirebaseListFromObj(object);
      const dummyRow = {...row};
      for (var objListKey in firebaseList) {
        row[getPrimaryKeyName(dummyRow)] = objListKey;
        const newRow = {...flattenObject(firebaseList[objListKey], row)};
        if (newRow && Object.keys(newRow).length > 0) {
          rowArray.push(newRow);
        }
      }
    } else if (isList(object)) {
      const firebaseObject = makeFirebaseListFromArr(object);
      for (var listKey in firebaseObject) {
        const dummyRow = {...row};
        dummyRow[getPrimaryKeyName(dummyRow, null, 'self')] = uuid();
        dummyRow._value = listKey;
        if (Object.keys(dummyRow).length > 0) {
          rowArray.push(dummyRow);
        }
      }
    } else {
      for (var objectKey in object) {
        const value = object[objectKey];
        if (value === null || !['Object', 'Array'].includes(value.constructor.name)) {
          row[objectKey] = value;
        } else if (['Object', 'Array'].includes(value.constructor.name)) {
          const pkeyMap = getParentPrimaryKeyMap(row);
          if (isList(value)) {
            const firebaseList = makeFirebaseListFromArr(value);
            tableDetectedCallback(
              null,
              {
                tableName: parent || tableName,
                name: objectKey,
                pkeys: pkeyMap,
                data: Object.keys(firebaseList).map(item => ({_value: item})),
              }
            );
          } else if (isObjectList(value)) {
            const firebaseList = makeFirebaseListFromObj(value);
            tableDetectedCallback(
              null,
              {
                tableName: parent || tableName,
                name: objectKey,
                pkeys: pkeyMap,
                data: handleTableCandidate(firebaseList, `${parent || tableName}_${objectKey}`, tableDetectedCallback, false),
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
      const firebaseObject = makeFirebaseListFromArr(obj);
      for (var listKey in firebaseObject) {
        rowArray.push({
          _value: listKey,
          _id: uuid(),
        });
      }
      return rowArray;
    }
    if (isRandomList(obj)) {
      for (var objKey in obj) {
        rowArray.push({
          _key: objKey,
          _value: obj[objKey],
          _id: uuid(),
        });
      }
      return rowArray;
    }
    throwError('Message: invalid JSON provided for node ' + tableName);
  }
  for (var id in obj) {
    const randomUUID = uuid();
    const initialRow = {_id: id};
    if (!isRootLevel) {
      initialRow._idself = randomUUID;
    }
    const flatRow = flattenObject(obj[id], initialRow);
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
          if (newItem._idself === undefined) {
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
  const topLevelTables = [];
  for (var tableName in db) {
    topLevelTables.push({
      _id: uuid(),
      __tableName: tableName.replace(/[^a-zA-Z0-9]/g, '_'),
    });
    tablesMap[tableName] = handleTableCandidate(
      db[tableName],
      tableName,
      generateNewTable,
      true
    );
  }
  tablesMap.__rootTables = topLevelTables;
  return tablesMap;
};

module.exports = handleFirebaseJson;

