export const getPathRoot = path => {
  return path.split('/')[1];
};
export const stripTrailingSlash = url => {
  if (url && url.endsWith('/')) {
    return url.slice(0, -1);
  }

  return url;
};
