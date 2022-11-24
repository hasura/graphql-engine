import { push, replace } from 'react-router-redux';

import globals from '../Globals';

const urlPrefix = globals.urlPrefix;
const appPrefix = urlPrefix !== '/' ? urlPrefix + '/data' : '/data';

const getValidUrl = path => {
  return urlPrefix !== '/' ? urlPrefix + path : path;
};

const _push = ({ pathname, query = {} }) =>
  push({
    pathname: getValidUrl(pathname),
    query: query,
  });

const _replace = ({ pathname, query = {} }) =>
  replace({
    pathname: getValidUrl(pathname),
    query: query,
  });

const updateQsHistory = (qs = window.encodeURI('?filters=[]')) => {
  window.history.pushState('', '', qs);
};

export { appPrefix, _push, _replace, getValidUrl, updateQsHistory };
