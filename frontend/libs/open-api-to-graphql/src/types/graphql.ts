// Copyright IBM Corp. 2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

/**
 * Custom type definitions for GraphQL.
 */

import {
  GraphQLObjectType,
  GraphQLScalarType,
  GraphQLInputObjectType,
  GraphQLList,
  GraphQLEnumType,
  GraphQLUnionType,
  GraphQLFieldResolver,
} from 'graphql';

export enum GraphQLOperationType {
  Query,
  Mutation,
  Subscription,
}

export type GraphQLType =
  | GraphQLObjectType
  | GraphQLInputObjectType
  | GraphQLList<any>
  | GraphQLUnionType
  | GraphQLEnumType
  | GraphQLScalarType;

type Arg = {
  type: any;
  description?: string;
};

export type Args = {
  [key: string]: Arg;
};

export type SubscriptionContext = {
  pubsub: any;
  [key: string]: any;
};

export type SubscriptionIterator = (
  root: object,
  args: object,
  context: SubscriptionContext,
  info?: object
) => AsyncIterable<string | string[]>;

export type Field<TSource, TContext, TArgs> = {
  type: GraphQLType;
  resolve?: GraphQLFieldResolver<TSource, TContext, TArgs>;
  subscribe?: GraphQLFieldResolver<TSource, SubscriptionContext, TArgs>;
  args?: Args;
  description: string;
};
