import { NavigateFunction } from 'react-router';
import { setLSItem } from '@hasura/shared/utils';
import { parse } from 'graphql';
import { LS_KEYS } from '@hasura/shared/types';

export const openInGraphiQL = (navigate: NavigateFunction, query: string) => {
  if (query) {
    setLSItem(LS_KEYS.graphiqlQuery, query);
  }

  navigate('/api/api-explorer?mode=rest');
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
