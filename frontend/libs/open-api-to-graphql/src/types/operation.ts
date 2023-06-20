// Copyright IBM Corp. 2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

/**
 * Type definitions for the objects created during preprocessing for every
 * operation in the OAS.
 */

import {
  Oas3,
  LinkObject,
  OperationObject,
  ParameterObject,
  ServerObject,
  SchemaObject,
} from './oas3';

import { GraphQLOperationType } from './graphql';

import {
  GraphQLScalarType,
  GraphQLObjectType,
  GraphQLInputObjectType,
  GraphQLList,
  GraphQLEnumType,
  GraphQLUnionType,
} from 'graphql';

import { HTTP_METHODS } from '../oas_3_tools';

export enum TargetGraphQLType {
  // scalars
  string = 'string',
  integer = 'integer',
  float = 'float',
  boolean = 'boolean',
  id = 'id',
  bigint = 'bigint',
  upload = 'upload',

  // JSON
  json = 'json',

  // non-scalars
  object = 'object',
  list = 'list',
  enum = 'enum',

  anyOfObject = 'anyOfObject',
  oneOfUnion = 'oneOfUnion',
}

export type DataDefinition = {
  // OAS-related:

  // Ideal name for the GraphQL type and is used with the schema to identify a specific GraphQL type
  preferredName: string;

  // The schema of the data type, why may have gone through some resolution, and is used with preferredName to identify a specific GraphQL type
  schema: SchemaObject;

  /**
   * Similar to the required property in object schemas but because of certain
   * keywords to combine schemas, e.g. "allOf", this resolves the required
   * property in all member schemas
   */
  required: string[];

  // The type GraphQL type this dataDefintion will be created into
  targetGraphQLType: TargetGraphQLType;

  // Collapsed link objects from all operations returning the same response data
  links: { [key: string]: LinkObject };

  /**
   * Data definitions of subschemas in the schema
   *
   * I.e. If the dataDef is a list type, the subDefinition is a reference to the
   * list item type
   *
   * Or if the dataDef is an object type, the subDefinitions are references to
   * the field types
   *
   * Or if the dataDef is a union type, the subDefinitions are references to
   * the member types
   */
  subDefinitions:
    | DataDefinition // For GraphQL list type
    | { [fieldName: string]: DataDefinition } // For GraphQL (input) object type
    | DataDefinition[]; // For GraphQL union type

  // GraphQL-related:

  // The potential name of the GraphQL type if it is created
  graphQLTypeName: string;

  // The potential name of the GraphQL input object type if it is created
  graphQLInputObjectTypeName: string;

  // The GraphQL type if it is created
  graphQLType?:
    | GraphQLObjectType
    | GraphQLList<any>
    | GraphQLUnionType
    | GraphQLEnumType
    | GraphQLScalarType;

  // The GraphQL input object type if it is created
  graphQLInputObjectType?: GraphQLInputObjectType | GraphQLList<any>;
};

export type Operation = {
  /**
   * The raw operation object from the OAS
   */
  operation: OperationObject;

  /**
   * Identifier of the operation - may be created by concatenating method & path
   */
  operationId: string;

  /**
   * A combination of the operation method and path (and the title of the OAS
   * where the operation originates from if multiple OASs are provided) in the
   * form of:
   *
   * {title of OAS (if applicable)} {method in ALL_CAPS} {path}
   *
   * Used for documentation and logging
   */
  operationString: string;

  /**
   * Human-readable description of the operation
   */
  description: string;

  /**
   * Tags of this operation
   */
  tags: string[];

  /**
   * URL path of this operation
   */
  path: string;

  /**
   * HTTP method for this operation
   */
  method: HTTP_METHODS;

  /**
   * Content-type of the request payload
   */
  payloadContentType?: string;

  /**
   * Information about the request payload (if any)
   */
  payloadDefinition?: DataDefinition;

  /**
   * Determines wheter request payload is required for the request
   */
  payloadRequired: boolean;

  /**
   * Content-type of the request payload
   */
  responseContentType?: string;

  /**
   * Information about the response payload
   */
  responseDefinition: DataDefinition;

  /**
   * List of parameters of the operation
   */
  parameters: ParameterObject[];

  /**
   * List of keys of security schemes required by this operation
   *
   * NOTE: Keys are sanitized
   * NOTE: Does not contain OAuth 2.0-related security schemes
   */
  securityRequirements: string[];

  /**
   * (Local) server definitions of the operation.
   */
  servers: ServerObject[];

  /**
   * Whether this operation should be placed in an authentication viewer
   * (cannot be true if "viewer" option passed to OpenAPI-to-GraphQL is false).
   */
  inViewer: boolean;

  /**
   * Type of root operation type, i.e. whether the generated field should be
   * added to the Query, Mutation, or Subscription root operation
   */
  operationType: GraphQLOperationType;

  /**
   * The success HTTP code, 200-299, destined to become a GraphQL object type
   */
  statusCode: string;

  /**
   * The OAS which this operation originated from
   */
  oas: Oas3;
};
