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

// checkIfSubscription is a method being added to prevent endpoints
// with subscriptions being created. See Issue (#628)
// using `any` here since 'operation' is not defined on the type DefinitionNode
const checkIfSubscription = (queryRootNode: any) => {
  return queryRootNode?.operation === 'subscription';
};

const checkIfAnonymousQuery = (queryRootNode: any) => {
  // It probably doesn't have to be this explicit
  return queryRootNode?.name === undefined;
};

// isQueryValid is a helper to validate a query that's being created
// this helps avoiding to creating endpoints with comments, empty spaces
// and queries that are subscriptions (see above comment for reference)
export const isQueryValid = (query: string) => {
  if (!query.trim()) {
    return false;
  }

  try {
    const parsedAST = parse(query);
    if (!parsedAST) {
      return false;
    }
    // making sure that there's only 1 definition in the query
    // query shouldn't be a subscription - server also throws an error for the same
    // also a check's in place to make sure that the query is named
    if (
      parsedAST?.definitions?.length > 1 ||
      checkIfSubscription(parsedAST.definitions[0]) ||
      checkIfAnonymousQuery(parsedAST.definitions[0])
    ) {
      return false;
    }
    return true;
  } catch {
    return false;
  }
};
