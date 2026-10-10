import {
  MetadataTable,
  QueryCollectionQuery,
  RestEndpoint,
  Source,
} from '@hasura/shared/types';

export type EndpointDefinition = {
  restEndpoint: RestEndpoint;
  query: QueryCollectionQuery;
};

export type Operation = {
  name: string;
  type: GraphQLType;
  args: Field[];
};

export type Field = {
  type: GraphQLType;
  name: string;
};

export type GraphQLType = {
  kind: string;
  name?: string;
  ofType?: GraphQLType;
};

export type Generator = {
  operationName: (source: Source, table: MetadataTable) => string;
  generator: (
    root: string,
    table: string,
    operation: Operation,
    microfiber: any,
    customTableName?: string,
  ) => EndpointDefinition;
};
