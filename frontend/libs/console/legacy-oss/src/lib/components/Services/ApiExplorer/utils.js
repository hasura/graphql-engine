import endpoints from '../../../Endpoints';

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

export const isValidGraphQLOperation = operation => {
  return (
    operation.name && operation.name.value && operation.operation === 'query'
  );
};

export const getGraphQLEndpoint = mode =>
  mode === 'relay' ? endpoints.relayURL : endpoints.graphQLUrl;
