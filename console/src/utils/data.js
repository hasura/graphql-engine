/**
 * Return an object with {key, value} pairs as {'the string value (name)', 'the occurance of the given name'}
 *
 * @param  {Array} list    the list which is converted to an dictionary
 * @return {Object}        the actually dictionary
 */
export const convertListToDict = list => {
  const newList = list instanceof Array ? list : [].concat(list);
  return newList.length > 1
    ? newList.reduce((prev, next) => {
      const accumulator =
          prev instanceof Object
            ? prev
            : {
              [prev]: 1,
            };
      return {
        ...accumulator,
        ...{
          [next]: accumulator[next] ? accumulator[next] + 1 : 1,
        },
      };
    })
    : {
      [newList[0]]: 1,
    };
};

/**
 * Return a list with object to bunch {keyName: keyValue}.
 * @param  {String} keyName  the name of the key which is present in [{object}].
 * @param  {String} keyValue the value of the given key in [{object}].
 * @param  {Array[Object]} list     the list of objects
 * @return {Object}          the {key, value} pairs.
 */
export const convertListToDictUsingKV = (keyName, keyValue, list) => {
  // if the list has more than one object then reduce it.
  return list.length > 1
    ? list.reduce((prev, next, index) => {
      // Initial object for the list.
      if (index === 1) {
        const newObj = {};
        newObj[prev[keyName]] = prev[keyValue];
        newObj[next[keyName]] = next[keyValue];
        return newObj;
      }
      // Other objects for the list.
      prev[next[keyName]] = next[keyValue];
      return prev;
    })
    : (() => {
      return list.length <= 0
        ? {}
        : (() => {
          const newObj = {};
          newObj[list[0][keyName]] = list[0][keyValue];
          return newObj;
        })();
    })();
};

/**
 * Returns a list of all duplicates in a given list.
 * @param  {[type]}  values [description]
 * @return {Boolean}        [description]
 */
export const listDuplicate = values => {
  const occuranceDict = convertListToDict(values);
  return Object.keys(occuranceDict).filter(key => occuranceDict[key] > 1);
};

/**
 * Return a value if the key is present else null.
 * @param  {Object} object the object which needs to parsed.
 * @param  {String} name   the string which is needed.
 * @return {Object}        the value of the given key.
 */
const getValueFromObject = (object, name) => {
  // check if the name is present ('as such') in the object.
  if (object && object[name]) {
    return object[name];
  } else if (object && (name.includes('[') && name.includes(']'))) {
    // name is consider as a key followed by array indices.
    const names = name
      .split(']')
      .join('')
      .split('[');
    let tempObj = object;
    for (let i = 0; i < names.length; i++) {
      const selector = names[i];
      if (!tempObj) {
        break;
      } else {
        tempObj = tempObj[selector];
      }
    }
    return tempObj;
  }
  return null;
};

/**
 * Return a value for a given queryString.
 * @param  {Object} object      the object which needs to be parsed.
 * @param  {String} queryString the keys which needs to traversed. eg: key1.key2.key3
 * @return {Object}             the value of the given queryString.
 */
export const recursiveGetValueFromObject = (object, queryString) => {
  const keys = queryString.split('.');
  let obj = object;
  for (let i = 0; i < keys.length; i++) {
    obj = getValueFromObject(obj, keys[i]);
    if (!obj) {
      break;
    }
  }
  return obj;
};

/**
 * Clones the given object and modifies the variable present in the
 * queryString location with the given value.
 * @param  {Object} object      the base object used.
 * @param  {String} queryString the string which is used to clone the object.
 * @param  {Object} value       the value assigned to the last variable of the
 *                              query object.
 * @return {Object}             clone of the object with the value changed.
 */
export const recursiveClone = (object, queryString, value) => {
  const layerClone = (innerObject, keys) => {
    if (keys.length === 1) {
      const newObj = {};
      newObj[keys[0]] = value;
      return newObj;
    }
    const newObj = { ...innerObject };
    newObj[keys[0]] = layerClone(newObj[keys[0]], keys.slice(1));
    return newObj;
  };
  const keys = queryString.split('.');
  return layerClone(object, keys);
};
