import { OperationDefinitionNode } from 'graphql';
import endpoints from '../../Endpoints';
import type { DataHeader } from '@hasura/shared/types';

export const getHeadersAsJSON = (
  headers: DataHeader[] = [],
): Record<string, string> => {
  const headerJSON = {};
  const nonEmptyHeaders = headers.filter((header) => {
    return header.key && header.selected;
  });

  nonEmptyHeaders.forEach((header) => {
    headerJSON[header.key] = header.value;
  });

  return headerJSON;
};

export const isValidGraphQLOperation = (
  operation: OperationDefinitionNode,
): boolean => {
  return Boolean(
    operation.name && operation.name.value && operation.operation === 'query',
  );
};

export const getGraphQLEndpoint = (mode) =>
  mode === 'relay' ? endpoints.relayURL : endpoints.graphQLUrl;

export const getGraphQLWebSocketEndpoint = (mode) =>
  mode === 'relay'
    ? endpoints.relayWebSocketURL
    : endpoints.graphQLWebSocketURL;
