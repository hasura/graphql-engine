import type { GraphQLArgument, GraphQLField, GraphQLInputField } from 'graphql';
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

export type Operation = GraphQLField<unknown, unknown>;

export type Field = GraphQLArgument | GraphQLInputField;

export type Generator = {
  operationName: (source: Source, table: MetadataTable) => string;
  generator: (
    root: string,
    table: string,
    operation: Operation,
    customTableName?: string,
  ) => EndpointDefinition;
};
