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
