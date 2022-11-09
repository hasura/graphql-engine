export type CollectionName = string;

export interface QueryCollectionEntry {
  /** Name of the query collection */
  name: CollectionName;
  /** List of queries */
  definition: {
    queries: QueryCollection[];
  };
  /** Comment */
  comment?: string;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#collectionquery
 */
export interface Query {
  name: string;
  query: string;
}

export interface QueryCollection {
  /** Name of the query collection */
  name: CollectionName;
  /** List of queries */
  definition: {
    queries: Query[];
  };
  /** Comment */
  comment?: string;
}
