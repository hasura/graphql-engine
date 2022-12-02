import globals from '../../../Globals';

const strippedCurrUrl = url => {
  if (globals.urlPrefix !== '/') {
    /* Replaces first occurence of the urlPrefix, hopefully it is started at that url */
    return url.replace(globals.urlPrefix, '');
  }
  return url;
};

export { strippedCurrUrl };
