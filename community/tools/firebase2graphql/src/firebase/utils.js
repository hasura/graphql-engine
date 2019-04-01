const getParentPrimaryKeyMap = obj => {
  const pkeyMap = {};
  for (var pkey in obj) {
    if (pkey.indexOf('_id') === 0) {
      pkeyMap[pkey] = obj[pkey];
    }
  }
  return pkeyMap;
};

const getLastPrimaryKey = (obj, index = 0, selfGenerated = '') => {
  const id = index === 0 ? `_id${selfGenerated}` : `_id${selfGenerated}_${index}`;
  const nextIndex = index === 0 ? 2 : index + 1;
  if (!obj[`_id_${nextIndex}`]) {
    return id;
  }
  getLastPrimaryKey(obj, nextIndex, selfGenerated);
};

const getPrimaryKeyName = (obj, index = 0, selfGenerated = '') => {
  const id = index === 0 ? `_id${selfGenerated}` : `_id${selfGenerated}_${index}`;
  const nextIndex = index === 0 ? 2 : index + 1;
  if (obj[id] === undefined) {
    return id;
  }
  return getPrimaryKeyName(obj, nextIndex, selfGenerated);
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
  if (obj.constructor.name === 'Array') {
    let arrayElementDataType = null;
    for (let _i = obj.length - 1; _i >= 0; _i--) {
      if (arrayElementDataType === null) {
        arrayElementDataType = typeof obj[_i];
      } else if (arrayElementDataType !== typeof obj[_i]) {
        return false;
      }
    }
    return true;
  }
  for (var objkey in obj) {
    if (obj[objkey] === null) {
      return false;
    }
    if (obj[objkey].constructor.name !== 'Boolean' || !obj[objkey]) {
      return false;
    }
  }
  return true;
};

const makeFirebaseListFromObj = obj => {
  if (obj.constructor.name === 'Array') {
    const firebaseList = {};
    for (var i = obj.length - 1; i >= 0; i--) {
      const element = obj[i];
      firebaseList[i.toString()] = element;
    }
    return firebaseList;
  }
  return obj;
};

const makeFirebaseListFromArr = obj => {
  if (obj.constructor.name === 'Array') {
    const firebaseList = {};
    for (var i = obj.length - 1; i >= 0; i--) {
      const element = obj[i];
      firebaseList[element] = true;
    }
    return firebaseList;
  }
  return obj;
};

const isObjectList = obj => {
  if (obj === null || obj === undefined) {
    return false;
  }
  const listChildStructure = {};
  const checkElementConsistency = element => {
    if (element === null) {
      return false;
    }
    if (typeof element !== 'object') {
      return false;
    }
    if (Object.keys(obj).length === 0) {
      return false;
    }
    for (var childKey in element) {
      if (!listChildStructure[childKey]) {
        if (element[childKey] !== null && element[childKey] !== undefined) {
          listChildStructure[childKey] = typeof element[childKey];
        }
      } else if (element[childKey] !== null && element[childKey] !== undefined) {
        if (typeof element[childKey] !== listChildStructure[childKey]) {
          return false;
        }
      }
    }
    return true;
  };
  if (obj.constructor.name === 'Array') {
    for (let _i = obj.length - 1; _i >= 0; _i--) {
      let element = obj[_i];
      let consistent = checkElementConsistency(element);
      if (!consistent) {
        return false;
      }
    }
    return true;
  }
  for (var key in obj) {
    const element = obj[key];
    let consistent = checkElementConsistency(element);
    if (!consistent) {
      return false;
    }
  }
  return true;
};

module.exports = {
  getParentPrimaryKeyMap,
  getLastPrimaryKey,
  getPrimaryKeyName,
  isRandomList,
  isList,
  isObjectList,
  makeFirebaseListFromObj,
  makeFirebaseListFromArr,
};
