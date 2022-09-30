import queryString from 'query-string';

export const parseQueryString = search => {
  return queryString.parse(search);
};
