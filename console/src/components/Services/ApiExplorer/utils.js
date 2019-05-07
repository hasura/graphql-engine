export const getHeadersAsJSON = (headers = []) => {
  const headerJSON = {};
  const nonEmptyHeaders = headers.filter(header => {
    return header.key && header.isActive;
  });

  nonEmptyHeaders.forEach(header => {
    headerJSON[header.key] = header.value;
  });

  return headerJSON;
};
<<<<<<< HEAD:console/src/components/ApiExplorer/utils.js

const parseJWTHeader = header => {
  let isJWTHeader;
  let matches = [];
  const parseBearer = /^(bearer) (.*)/gim;
  if (header.key.toLowerCase() === 'authorization') {
    matches = parseBearer.exec(header.value);
    if (matches && matches[1] === 'Bearer') {
      isJWTHeader = true;
    }
  }
  return { isJWTHeader, matches };
};

export { getHeadersAsJSON, parseJWTHeader };
=======
>>>>>>> master:console/src/components/Services/ApiExplorer/utils.js
