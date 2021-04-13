import {
  inconsistentObjectsQuery,
  getReloadMetadataQuery,
  getReloadRemoteSchemaCacheQuery,
} from './queryUtils';
import { AllowList, QueryCollectionEntry, HasuraMetadataV3 } from './types';
import { AllowedQueriesCollection } from './reducer';

export const allowedQueriesCollection = 'allowed-queries';

export const findAllowedQueryCollections = (
  collectionName: string,
  allowList: AllowList[]
) => {
  return allowList.find(
    allowedCollection => collectionName === allowedCollection.collection
  );
};

export const setAllowedQueries = (
  allQueryCollections?: QueryCollectionEntry[],
  allowlist?: AllowList[]
): AllowedQueriesCollection[] => {
  if (!allQueryCollections || !allowlist) return [];
  const allowedQueryCollections = allQueryCollections.filter(query =>
    findAllowedQueryCollections(query.name, allowlist)
  );

  const allowedQueries: AllowedQueriesCollection[] = [];
  allowedQueryCollections.forEach(collection => {
    collection.definition.queries.forEach(query => {
      allowedQueries.push({ ...query, collection: collection.name });
    });
  });
  return allowedQueries;
};

export const deleteAllowedQueryQuery = (
  queryName: string,
  collectionName = allowedQueriesCollection
) => ({
  type: 'drop_query_from_collection',
  args: {
    collection_name: collectionName,
    query_name: queryName,
  },
});

export const addAllowedQuery = (
  query: { name: string; query: string },
  collectionName = allowedQueriesCollection
) => ({
  type: 'add_query_to_collection',
  args: {
    collection_name: collectionName,
    query_name: query.name,
    query: query.query,
  },
});

export const updateAllowedQueryQuery = (
  queryName: string,
  newQuery: { name: string; query: string },
  collectionName = allowedQueriesCollection
) => ({
  type: 'bulk',
  args: [
    deleteAllowedQueryQuery(queryName, collectionName),
    addAllowedQuery(newQuery, collectionName),
  ],
});

export const deleteAllowListQuery = (
  collectionName = allowedQueriesCollection
) => ({
  type: 'drop_query_collection',
  args: {
    collection: collectionName,
    cascade: true,
  },
});

export const addAllowedQueriesQuery = (
  queries: Array<{ name: string; query: string }>,
  source: string
) => {
  const addQueries = queries.map(query => addAllowedQuery(query));

  return {
    type: 'bulk',
    source,
    args: addQueries,
  };
};

export const createAllowListQuery = (
  queries: Array<{ name: string; query: string }>,
  source: string
) => {
  const createAllowListCollectionQuery = {
    type: 'create_query_collection',
    args: {
      name: allowedQueriesCollection,
      definition: {
        queries,
      },
    },
  };

  const addCollectionToAllowListQuery = {
    type: 'add_collection_to_allowlist',
    args: {
      collection: allowedQueriesCollection,
    },
  };

  return {
    type: 'bulk',
    source,
    args: [createAllowListCollectionQuery, addCollectionToAllowListQuery],
  };
};

export const reloadRemoteSchemaCacheAndGetInconsistentObjectsQuery = (
  remoteSchemaName: string,
  source: string
) => {
  return {
    type: 'bulk',
    source,
    args: [
      getReloadRemoteSchemaCacheQuery(remoteSchemaName),
      inconsistentObjectsQuery,
    ],
  };
};

export const getReloadCacheAndGetInconsistentObjectsQuery = (
  shouldReloadRemoteSchemas: boolean,
  source: string
) => ({
  type: 'bulk',
  source,
  args: [
    getReloadMetadataQuery(shouldReloadRemoteSchemas),
    inconsistentObjectsQuery,
  ],
});

export const addInheritedRole = (roleName: string, roleSet: string[]) => ({
  type: 'add_inherited_role',
  args: {
    role_name: roleName,
    role_set: roleSet,
  },
});

export const deleteInheritedRole = (roleName: string) => ({
  type: 'drop_inherited_role',
  args: {
    role_name: roleName,
  },
});

export const updateInheritedRole = (roleName: string, roleSet: string[]) => ({
  type: 'bulk',
  args: [deleteInheritedRole(roleName), addInheritedRole(roleName, roleSet)],
});

export const isMetadataEmpty = (metadataObject: HasuraMetadataV3) => {
  const { actions, sources, remote_schemas } = metadataObject;
  const hasRemoteSchema = remote_schemas && remote_schemas.length;
  const hasAction = actions && actions.length;
  const hasTable = sources.some(source => source.tables.length);
  return !(hasRemoteSchema || hasAction || hasTable);
};

export const hasSources = (metadataObject: HasuraMetadataV3) => {
  return metadataObject?.sources?.length > 0;
};
