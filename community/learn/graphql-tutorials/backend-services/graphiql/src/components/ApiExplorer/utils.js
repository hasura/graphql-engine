const getHeadersAsJSON = headers => {
  const headerJSON = {};
  const nonEmptyHeaders = headers.filter(header => {
    return (header.key || header.value) && header.isActive;
  });
  nonEmptyHeaders.forEach(header => {
    headerJSON[header.key] = header.value;
  });
  return headerJSON;
};

const isReactNative = () => {
  const urlParams = new URLSearchParams(window.location.search);
  if (urlParams.get('tutorial') === 'react-native') {
    return true;
  }
  return false;
};

const emailRegex = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/; 

export { getHeadersAsJSON, isReactNative, emailRegex };
