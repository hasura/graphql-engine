export type CollectionName = string;

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#collectionquery
 */
export interface QueryCollectionQuery {
  name: string;
  query: string;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/query-collections.html#args-syntax
 */
export interface QueryCollection {
  /** Name of the query collection */
  name: CollectionName;
  /** List of queries */
  definition: {
    queries: QueryCollectionQuery[];
  };
  /** Comment */
  comment?: string;
}
