// Copyright IBM Corp. 2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

// Type imports:
import { GraphQLOperationType, SubscriptionContext } from './graphql';
import { GraphQLFieldResolver, GraphQLResolveInfo } from 'graphql';
import crossFetch from 'cross-fetch';
import FormData from 'form-data';

/**
 * Type definition of the options that users can pass to OpenAPI-to-GraphQL.
 */
export type Warning = {
  type: string;
  message: string;
  mitigation: string;
  path?: string[];
};

export type Report = {
  validationErrors?: string[];
  warnings: Warning[];
  numOps: number;
  numOpsQuery: number;
  numOpsMutation: number;
  numOpsSubscription: number;
  numQueriesCreated: number;
  numMutationsCreated: number;
  numSubscriptionsCreated: number;
};

export type ConnectOptions = {
  [key: string]: boolean | number | string;
};

// A standardized type that can be used to identify a specific operation in an OAS
export type OasTitlePathMethodObject<T> = {
  [title: string]: {
    [path: string]: {
      [method: string]: T;
    };
  };
};

/**
 * Given a set parameters corresponding to a specific operation in the OAS,
 * provide the appropriate headers
 */
export type RequestHeadersFunction<TSource, TContext, TArgs> = (
  method: string,
  path: string,
  title: string,
  resolverParams?: {
    source: TSource;
    args: TArgs;
    context: TContext;
    info: GraphQLResolveInfo;
  }
) => HeadersInit;

/**
 * We rely on the Request library in order to make resolver API calls.
 *
 * We expose the options so that users can have more control over the calls.
 *
 * We modify the RequestOptions so that headers can also be provided as a
 * function.
 *
 * Based on: https://github.com/request/request#requestoptions-callback
 */
export type RequestOptions<TSource, TContext, TArgs> = Omit<
  RequestInit,
  'headers'
> & {
  headers?: HeadersInit | RequestHeadersFunction<TSource, TContext, TArgs>;
  qs?: Record<string, string>;
};

/**
 * We use the form-data library to prepare multipart requests within the resolver API calls,
 * also it provides support for handling file as streams this way the file upload has a minimal memory footprint,
 * unlike the situation where the entire file in memory initially.
 *
 * Provides accommodation to allow overrides or add options for how the form data representation for multipart requests is generated
 *
 * Based on: https://github.com/form-data/form-data#custom-options
 */
export type FileUploadOptions = ConstructorParameters<typeof FormData>[0];

export type Options<TSource, TContext, TArgs> = Partial<
  InternalOptions<TSource, TContext, TArgs>
>;

export type InternalOptions<TSource, TContext, TArgs> = {
  /*
   * Adhere to the OAS as closely as possible. If set to true, any deviation
   * from the OAS will lead OpenAPI-to-GraphQL to throw.
   */
  strict: boolean;

  /**
   * Holds information about the GraphQL schema generation process
   */
  report: Report;

  // Schema options

  /**
   * Field names can only be sanitized operationIds
   *
   * By default, query field names are based on the return type type name and
   * mutation field names are based on the operationId, which may be generated
   * if it does not exist.
   *
   * This option forces OpenAPI-to-GraphQL to only create field names based on the
   * operationId.
   */
  operationIdFieldNames: boolean;

  /**
   * Under certain circumstances (such as response code 204), some RESTful
   * operations should not return any data. However, GraphQL objects must have
   * a data structure. Normally, these operations would be ignored but for the
   * sake of completeness, the following option will give these operations a
   * placeholder data structure. Even though the data structure will not have
   * any practical use, at least the operations will show up in the schema.
   */
  fillEmptyResponses: boolean;

  /**
   * Auto-generate a 'limit' argument for all fields that return lists of
   * objects, including ones produced by links
   *
   * Allows to constrain the return size of lists of objects
   *
   * Returns the first n number of elements in the list
   */
  addLimitArgument: boolean;

  /**
   * If a schema is of type string and has format UUID, it will be translated
   * into a GraphQL ID type. To allow for more customzation, this option allows
   * users to specify other formats that should be interpreted as ID types.
   */
  idFormats?: string[];

  /**
   * Allows to define the root operation type (Query or Mutation type) of any
   * OAS operation explicitly.
   *
   * OpenAPI-to-GraphQL will by default make all GET operations Query fields and all other
   * operations into Mutation fields.
   *
   * The field is identifed first by the title of the OAS, then the path of the
   * operation, and lastly the method of the operation.
   */
  selectQueryOrMutationField?: OasTitlePathMethodObject<GraphQLOperationType>;

  /**
   * Sets argument name for the payload of a mutation to 'requestBody'
   */
  genericPayloadArgName: boolean;

  /**
   * By default, field names are sanitized to conform with GraphQL conventions,
   * i.e. types should be in PascalCase, fields should be in camelCase, and
   * enum values should be in ALL_CAPS.
   *
   * This option will prevent OpenAPI-to-GraphQL from enforcing camelCase field names and
   * PascalCase type names, only removing illegal characters and staying as true
   * to the provided names in the OAS as possible.
   */
  simpleNames: boolean;

  /**
   * By default, field names are sanitized to conform with GraphQL conventions,
   * i.e. types should be in PascalCase, fields should be in camelCase, and
   * enum values should be in ALL_CAPS.
   *
   * This option will prevent OpenAPI-to-GraphQL from enforcing ALL_CAPS enum
   * values, only removing illegal characters and staying as true to the
   * provided enum values in the OAS as possible.
   */
  simpleEnumValues: boolean;

  /**
   * Experimental feature that will try to create more meaningful names from
   * the operation path than the response object by leveraging common
   * conventions.
   *
   * For example, given the operation GET /users/{userId}/car, OpenAPI-to-GraphQL will
   * create a Query field 'userCar'. Note that because 'users' is followed by
   * the parameter 'userId', it insinuates that this operation will get the car
   * that belongs to a singular user. Hence, the name 'userCar' is more fitting
   * than 'usersCar' so the pluralizing 's' is dropped.
   *
   * This option will also consider irregular plural forms.
   */
  singularNames: boolean;

  /**
   * Allow to generate subscription fields from callback objects in the OAS.
   *
   * The keys (runtime expressions) of the callback object will be interpolated
   * as the topic of publish/subscription connection.
   */
  createSubscriptionsFromCallbacks: boolean;

  // Resolver options

  /**
   * Custom headers to send with every request made by a resolve function.
   */
  headers?: HeadersInit | RequestHeadersFunction<TSource, TContext, TArgs>;

  /**
   * Custom query parameters to send with every reqeust by a resolve function.
   */
  qs?: { [key: string]: string };

  /**
   * We use the Request library to make calls to the API backend.
   *
   * Allows to override or add options to the API calls, e.g. setup the web
   * proxy to use.
   *
   * Headers can either be provided as an object or a function that returns
   * an object.
   *
   * Based on: https://github.com/request/request#requestoptions-callback
   */
  requestOptions?: Partial<RequestOptions<TSource, TContext, TArgs>>;

  /**
   * Allows to override or add options to the PubSub connect object used to make
   * publish/subscribe to the API backend.
   * e.g. Setup the web proxy to use.
   */
  connectOptions?: ConnectOptions;

  /**
   * Specifies the URL on which all paths will be based on.
   * Overrides the server object in the OAS.
   */
  baseUrl?: string;

  /**
   * Allows to define custom resolvers for fields on the Query/Mutation root
   * operation type.
   *
   * In other words, instead of resolving on an operation (REST call) defined in
   * the OAS, the field will resolve on the custom resolver. Note that this will
   * also affect the behavior of links.
   *
   * The field is identifed first by the title of the OAS, then the path of the
   * operation, and lastly the method of the operation.
   *
   * Use cases include the resolution of complex relationships between types,
   * implementing performance improvements like caching, or dealing with
   * non-standard authentication requirements.
   */
  customResolvers?: OasTitlePathMethodObject<
    GraphQLFieldResolver<TSource, TContext, TArgs>
  >;

  /**
   * Allows to define custom resolvers and subscribe functions for fields on the
   * Subscription root operation type.
   *
   * In other words, instead of resolving on an operation (REST call) defined in
   * the OAS, the field will resolve on the custom resolver. Note that this will
   * also affect the behavior of links.
   *
   * The field is identifed first by the title of the OAS, then the path of the
   * operation, and lastly the method of the operation.
   *
   * Use cases include the resolution of complex relationships between types,
   * implementing performance improvements like caching, or dealing with
   * non-standard authentication requirements.
   *
   * Note: Subscription fields will only be generated if the
   * createSubscriptionsFromCallbacks option is enabled.
   */
  customSubscriptionResolvers?: OasTitlePathMethodObject<{
    subscribe: GraphQLFieldResolver<TSource, SubscriptionContext, TArgs>;
    resolve: GraphQLFieldResolver<TSource, TContext, TArgs>;
  }>;

  /**
   * Allows one to define config for the form data that will be used in streaming
   * the uploaded file from the client to the intending endpoint
   *
   * Based on: https://github.com/form-data/form-data#custom-options
   */
  fileUploadOptions?: FileUploadOptions;

  // Authentication options

  /**
   * Determines whether OpenAPI-to-GraphQL should create viewers that allow users to pass
   * basic auth and API key credentials.
   */
  viewer: boolean;

  /**
   * JSON path to OAuth 2 token contained in GraphQL context. Tokens will per
   * default be sent in "Authorization" header.
   */
  tokenJSONpath?: string;

  /**
   * Determines whether to send OAuth 2 token as query parameter instead of in
   * header.
   */
  sendOAuthTokenInQuery: boolean;

  // Validation options

  /**
   * We use the oas-validator library to validate Swaggers/OASs.
   *
   * We expose the options so that users can have more control over validation.
   *
   * Based on: https://github.com/Mermade/oas-kit/blob/master/docs/options.md
   */
  oasValidatorOptions: object;

  /**
   * We use the swagger2graphql library to translate Swaggers to OASs.
   *
   * We expose the options so that users can have more control over translation.
   *
   * Based on: https://github.com/Mermade/oas-kit/blob/master/docs/options.md
   */
  swagger2OpenAPIOptions: object;

  /**
   * Determines how to perform validation of the OAS. If set to true, the
   * process will throw an error and stop execution. If set to false, the
   * process will return the error in the response and continue execution.
   */
  softValidation?: boolean;

  // Logging options

  /**
   * The error extensions is part of the GraphQLErrors that will be returned if
   * the query cannot be fulfilled. It provides information about the failed
   * REST call(e.g. the method, path, status code, response
   * headers, and response body). It can be useful for debugging but may
   * unintentionally leak information.
   *
   * This option prevents the extensions from being created.
   */

  provideErrorExtensions: boolean;

  /**
   * Appends a small statement to the end of field description that clarifies
   * the operation that the field will trigger.
   *
   * Will affect query and mutation fields as well as fields created from links
   *
   * In the form of: 'Equivalent to {title of OAS} {method in ALL_CAPS} {path}'
   * Will forgo the title is only one OAS is provided
   */
  equivalentToMessages: boolean;

  /**
   * Custom W3 Compatible `fetch` implementation
   */
  fetch: typeof crossFetch;
};
