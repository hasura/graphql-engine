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

module.exports = {
  getParentPrimaryKeyMap,
  getLastPrimaryKey,
  getPrimaryKeyName,
  isRandomList,
  isList,
  isObjectList,
};
