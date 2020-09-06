import {
  inconsistentObjectsQuery,
  getReloadMetadataQuery,
  getReloadRemoteSchemaCacheQuery,
} from './queryUtils';

export const allowedQueriesCollection = 'allowed-queries';

export const deleteAllowedQueryQuery = (queryName: string) => ({
  type: 'drop_query_from_collection',
  args: {
    collection_name: allowedQueriesCollection,
    query_name: queryName,
  },
});
const addAllowedQuery = (query: { name: string; query: string }) => ({
  type: 'add_query_to_collection',
  args: {
    collection_name: allowedQueriesCollection,
    query_name: query.name,
    query: query.query,
  },
});

export const updateAllowedQueryQuery = (
  queryName: string,
  newQuery: { name: string; query: string }
) => ({
  type: 'bulk',
  args: [deleteAllowedQueryQuery(queryName), addAllowedQuery(newQuery)],
});

export const deleteAllowListQuery = () => ({
  type: 'drop_query_collection',
  args: {
    collection: allowedQueriesCollection,
    cascade: true,
  },
});

export const addAllowedQueriesQuery = (
  queries: Array<{ name: string; query: string }>
) => {
  const addQueries = queries.map(query => addAllowedQuery(query));

  return {
    type: 'bulk',
    args: addQueries,
  };
};

export const createAllowListQuery = (
  queries: Array<{ name: string; query: string }>
) => {
  const createAllowListCollectionQuery = () => ({
    type: 'create_query_collection',
    args: {
      name: allowedQueriesCollection,
      definition: {
        queries,
      },
    },
  });

  const addCollectionToAllowListQuery = () => ({
    type: 'add_collection_to_allowlist',
    args: {
      collection: allowedQueriesCollection,
    },
  });

  return {
    type: 'bulk',
    args: [createAllowListCollectionQuery(), addCollectionToAllowListQuery()],
  };
};

export const reloadRemoteSchemaCacheAndGetInconsistentObjectsQuery = (
  remoteSchemaName: string
) => {
  return {
    type: 'bulk',
    args: [
      getReloadRemoteSchemaCacheQuery(remoteSchemaName),
      inconsistentObjectsQuery,
    ],
  };
};

export const getReloadCacheAndGetInconsistentObjectsQuery = (
  shouldReloadRemoteSchemas: boolean
) => ({
  type: 'bulk',
  args: [
    getReloadMetadataQuery(shouldReloadRemoteSchemas),
    inconsistentObjectsQuery,
  ],
});
