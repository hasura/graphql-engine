import globals from '../Globals';
import { loadPATState } from '../components/AppState';

export const constructRedirectUrl = (pathname, search) => {
  let finalUrl = '';
  if (pathname) {
    finalUrl += pathname;
  }
  if (search) {
    finalUrl += search;
  }
  return finalUrl;
};

export const isProloginWithPAT = () => {
  return loadPATState() || globals.personalAccessToken;
};

export const isJsonString = str => {
  try {
    JSON.parse(str);
  } catch (e) {
    return false;
  }
  return true;
};
