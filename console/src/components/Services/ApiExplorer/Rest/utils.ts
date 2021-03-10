import { parse } from 'graphql';

import globals from '../../../../Globals';
import { AllowedRESTMethods } from '../../../../metadata/types';

// getCurrentPageHost can be used within all components
// that are showing the REST endpoint that will be generated
export const getCurrentPageHost = () => {
  const pageProtocol = window.location.protocol;
  const urlHost = !globals.dataApiUrl
    ? window.location.host
    : globals.dataApiUrl;
  return globals.dataApiUrl ?? `${pageProtocol}://${urlHost}`;
};

const positionsMap = {
  GET: 1,
  POST: 2,
  PUT: 3,
  PATCH: 4,
  DELETE: 5,
};

const modifyMethodsList = (methods: AllowedRESTMethods[]) =>
  methods.map(method => ({ name: method, position: positionsMap[method] }));

// badgeSort is applied when displaying badges
// so that they come in the same order GET, POST, PUT, PATCH, DELETE
export const badgeSort = (methods: AllowedRESTMethods[]) => {
  const modifiedMethods = modifyMethodsList(methods);
  return modifiedMethods
    .sort((a, b) => a.position - b.position)
    .map(method => method.name);
};

// checkIfSubscription is a temporary method being added to prevent endpoints
// with subscriptions being created. See Issue (#628)
// using `any` here since 'operation' is not defined on the type DefinitionNode
const checkIfSubscription = (queryRootNode: any) => {
  return queryRootNode?.operation === 'subscription';
};

// checkValidQuery is a helper to validate a query that's being created
// this helps avoiding to creating endpoints with comments, empty spaces
// and queries that are subscriptions (see above comment for reference)
export const isQueryValid = (query: string) => {
  if (!query.trim()) {
    return false;
  }

  try {
    const parsedAST = parse(query);
    return !checkIfSubscription(parsedAST.definitions[0]);
  } catch {
    return false;
  }
};
