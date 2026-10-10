import globals from '../Globals';

const urlPrefix = globals.urlPrefix;
const appPrefix = urlPrefix !== '/' ? urlPrefix + '/data' : '/data';

const getValidUrl = (path) => {
  return urlPrefix !== '/' ? urlPrefix + path : path;
};

const updateQsHistory = (qs = window.encodeURI('?filters=[]')) => {
  window.history.pushState('', '', qs);
};

export { appPrefix, getValidUrl, updateQsHistory };
