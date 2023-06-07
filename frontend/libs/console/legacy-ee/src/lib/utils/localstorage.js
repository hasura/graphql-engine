let localStorageAvailable = null;

/**
 * Set a given (key, value) to a given store.
 * @param {String} storeName the name of the store, note that the name can be pageName
 * @param {String} key       the name of the key which is to be added. If the key is
 * present then it overwritten.
 * @param {String} value     the value for the given key.
 */
export const setStore = (storeName, key, value) => {
  if (localStorage.getItem(storeName)) {
    const prevObj = JSON.parse(localStorage.getItem(storeName));
    prevObj[key] = value;
    localStorage.setItem(storeName, JSON.stringify(prevObj));
  } else {
    const newObj = {};
    newObj[key] = value;
    localStorage.setItem(storeName, JSON.stringify(newObj));
  }
  return true;
};

/**
 * Get a value if the key and storeName is present. If the storeName is only present
 * then it return what is all keys on the store.
 * @param  {String} storeName  the name of the store.
 * @param  {String} [key=null] the key of the store. If the key is null by default.
 * @return {Object}            The Object with either just the value.
 */
export const getStore = (storeName, key = null) => {
  if (localStorage.getItem(storeName)) {
    const obj = JSON.parse(localStorage.getItem(storeName));
    if (!key) {
      return obj;
    }
    return obj[key] !== undefined ? obj[key] : null;
  }
  return null;
};

/**
 * Remove the given key 'or' storeName from the localStorage. If the key is null
 * then the store is removed.
 * @param  {String} storeName  the name of the store.
 * @param  {String} [key=null] the name of the key in the store. Null by default.
 * @return {Object}            {success: boolean, message: 'string'}
 */
export const removeStore = (storeName, key = null) => {
  if (localStorage.getItem(storeName)) {
    if (!key) {
      localStorage.removeItem(storeName);
      return { success: true, message: 'store removed.' };
    }
    const obj = JSON.parse(localStorage.getItem(storeName));
    delete obj[key];
    localStorage.setItem(storeName, JSON.stringify(obj));
    return { success: true, message: 'key from store removed.' };
  }
  return { success: false, message: 'store not found.' };
};

/**
 * Check if the given browser is compatiable with localStorage.
 * @return {boolean} if the localStorage is supported.
 */
export const initializeStore = () => {
  if (localStorageAvailable === null) {
    if (typeof Storage !== 'undefined') {
      // Code for localStorage/sessionStorage.
      localStorageAvailable = true;
    } else {
      // Sorry! No Web Storage support..
      alert(
        "Sorry, it seems you don't have support for local storage. Which means we can't save user-preferences to your browser."
      );
      localStorageAvailable = false;
    }
  }
  return localStorageAvailable;
};
