// Copyright IBM Corp. 2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

/**
 * Functions to create resolve functions.
 */

// Type imports:
import { SchemaObject, ParameterObject } from './types/oas3';
import { ConnectOptions } from './types/options';
import { TargetGraphQLType, Operation } from './types/operation';
import { SubscriptionContext } from './types/graphql';
import { PreprocessingData } from './types/preprocessing_data';
import { RequestOptions, FileUploadOptions } from './types/options';
import crossFetch from 'cross-fetch';
import { FileUpload } from 'graphql-upload/Upload';

// Imports:
import stream from 'stream';
import * as Oas3Tools from './oas_3_tools';
import { JSONPath } from 'jsonpath-plus';
import * as JSONPointer from 'jsonpointer';
import { debug } from 'debug';
import { GraphQLError, GraphQLFieldResolver } from 'graphql';
import formurlencoded from 'form-urlencoded';
import { PubSub } from 'graphql-subscriptions';
import urljoin from 'url-join';
import FormData from 'form-data';

const pubsub = new PubSub();

const translationLog = debug('translation');
const httpLog = debug('http');
const pubsubLog = debug('pubsub');
const uploadLog = debug('fileUpload');

// OAS runtime expression reference locations
const RUNTIME_REFERENCES = ['header.', 'query.', 'path.', 'body'];

export const OPENAPI_TO_GRAPHQL = '_openAPIToGraphQL';

// Type definitions & exports:
type AuthReqAndProtcolName = {
  authRequired: boolean;
  securityRequirement?: string;
  sanitizedSecurityRequirement?: string;
};

type AuthOptions = {
  authHeaders: { [key: string]: string };
  authQs: { [key: string]: string };
  authCookie: string;
};

type GetResolverParams<TSource, TContext, TArgs> = {
  operation: Operation;
  argsFromLink?: { [key: string]: any };
  payloadName?: string;
  responseName?: string;
  data: PreprocessingData<TSource, TContext, TArgs>;
  baseUrl?: string;
  requestOptions?: Partial<RequestOptions<TSource, TContext, TArgs>>;
  fileUploadOptions?: FileUploadOptions;
  fetch: typeof crossFetch;
};

type inferLinkArgumentsParam<TSource, TContext, TArgs> = {
  paramName: string;
  value: any;
  resolveData: Partial<ResolveData<TSource, TContext, TArgs>>;
  source: TSource;
  args: TArgs;
};

type GetSubscribeParams<TSource, TContext, TArgs> = {
  operation: Operation;
  argsFromLink?: { [key: string]: string };
  payloadName?: string;
  data: PreprocessingData<TSource, TContext, TArgs>;
  baseUrl?: string;
  connectOptions?: ConnectOptions;
};

type ResolveData<TSource, TContext, TArgs> = {
  url: string;
  usedParams: any;
  usedPayload: any;
  usedRequestOptions: RequestOptions<TSource, TContext, TArgs>;
  usedStatusCode: string;
  responseHeaders: HeadersInit;
};

// TODO: Determine better name
type OpenAPIToGraphQLRoot<TSource, TContext, TArgs> = {
  data?: {
    [identifier: string]: ResolveData<TSource, TContext, TArgs>;
  };

  /**
   * TODO: We can define more specific types. See getProcessedSecuritySchemes().
   *
   * Is it related TArgs?
   */
  security: { [saneProtocolName: string]: any };
};

// TODO: Determine better name
type OpenAPIToGraphQLSource<TSource, TContext, TArgs> = {
  _openAPIToGraphQL: OpenAPIToGraphQLRoot<TSource, TContext, TArgs>;
};

/*
 * If the operation type is Subscription, create and return a resolver object
 * that contains subscribe to perform subscription and resolve to execute
 * payload transformation
 */
export function getSubscribe<TSource, TContext, TArgs extends object>({
  operation,
  payloadName,
  data,
  baseUrl,
  connectOptions,
}: GetSubscribeParams<TSource, TContext, TArgs>): GraphQLFieldResolver<
  TSource,
  SubscriptionContext,
  TArgs
> {
  // Determine the appropriate URL:
  if (typeof baseUrl === 'undefined') {
    baseUrl = Oas3Tools.getBaseUrl(operation);
  }

  // Return custom resolver if it is defined
  const customResolvers = data.options.customSubscriptionResolvers;
  const title = operation.oas.info.title;
  const path = operation.path;
  const method = operation.method;

  if (
    typeof customResolvers === 'object' &&
    typeof customResolvers[title] === 'object' &&
    typeof customResolvers[title][path] === 'object' &&
    typeof customResolvers[title][path][method] === 'object' &&
    typeof customResolvers[title][path][method].subscribe === 'function'
  ) {
    translationLog(
      `Use custom publish resolver for ${operation.operationString}`
    );

    return customResolvers[title][path][method].subscribe;
  }

  return (root, args, context, info) => {
    /**
     * Determine possible topic(s) by resolving callback path
     *
     * GraphQL produces sanitized payload names, so we have to sanitize before
     * lookup here
     */
    const paramName = Oas3Tools.sanitize(
      payloadName,
      Oas3Tools.CaseStyle.camelCase
    );

    let resolveData: any = {};

    if (payloadName && typeof payloadName === 'string') {
      // The option genericPayloadArgName will change the payload name to "requestBody"
      const sanePayloadName = data.options.genericPayloadArgName
        ? 'requestBody'
        : Oas3Tools.sanitize(payloadName, Oas3Tools.CaseStyle.camelCase);

      if (sanePayloadName in args) {
        if (typeof args[sanePayloadName] === 'object') {
          const rawPayload = Oas3Tools.desanitizeObjectKeys(
            args[sanePayloadName],
            data.saneMap
          );
          resolveData.usedPayload = rawPayload;
        } else {
          const rawPayload = JSON.parse(args[sanePayloadName]);
          resolveData.usedPayload = rawPayload;
        }
      }
    }

    if (connectOptions) {
      resolveData.usedRequestOptions = connectOptions;
    } else {
      resolveData.usedRequestOptions = {
        method: resolveData.usedPayload.method
          ? resolveData.usedPayload.method
          : method.toUpperCase(),
      };
    }

    pubsubLog(
      `Subscription schema: ${JSON.stringify(resolveData.usedPayload)}`
    );

    let value = path;
    let paramNameWithoutLocation = paramName;
    if (paramName.indexOf('.') !== -1) {
      paramNameWithoutLocation = paramName.split('.')[1];
    }

    // See if the callback path contains constants expression
    if (value.search(/{|}/) === -1) {
      args[paramNameWithoutLocation] = isRuntimeExpression(value)
        ? resolveRuntimeExpression(paramName, value, resolveData, root, args)
        : value;
    } else {
      // Replace callback expression with appropriate values
      const cbParams = value.match(/{([^}]*)}/g);
      pubsubLog(`Analyzing subscription path: ${cbParams.toString()}`);

      cbParams.forEach(cbParam => {
        value = value.replace(
          cbParam,
          resolveRuntimeExpression(
            paramName,
            cbParam.substring(1, cbParam.length - 1),
            resolveData,
            root,
            args
          )
        );
      });
      args[paramNameWithoutLocation] = value;
    }

    const topic = args[paramNameWithoutLocation] || 'test';
    pubsubLog(`Subscribing to: ${topic}`);
    return context.pubsub
      ? context.pubsub.asyncIterator(topic)
      : pubsub.asyncIterator(topic);
  };
}

/*
 * If the operation type is Subscription, create and return a resolver function
 * triggered after a message has been published to the corresponding subscribe
 * topic(s) to execute payload transformation
 */
export function getPublishResolver<TSource, TContext, TArgs>({
  operation,
  responseName,
  data,
}: GetResolverParams<TSource, TContext, TArgs>): GraphQLFieldResolver<
  TSource,
  TContext,
  TArgs
> {
  // Return custom resolver if it is defined
  const customResolvers = data.options.customSubscriptionResolvers;
  const title = operation.oas.info.title;
  const path = operation.path;
  const method = operation.method;

  if (
    typeof customResolvers === 'object' &&
    typeof customResolvers[title] === 'object' &&
    typeof customResolvers[title][path] === 'object' &&
    typeof customResolvers[title][path][method] === 'object' &&
    typeof customResolvers[title][path][method].resolve === 'function'
  ) {
    translationLog(
      `Use custom publish resolver for ${operation.operationString}`
    );

    return customResolvers[title][path][method].resolve;
  }

  return (payload, args, context, info) => {
    // Validate and format based on operation.responseDefinition
    const typeOfResponse = operation.responseDefinition.targetGraphQLType;
    pubsubLog(
      `Message received: ${responseName}, ${typeOfResponse}, ${JSON.stringify(
        payload
      )}`
    );

    let responseBody;
    let saneData;

    if (typeof payload === 'object') {
      if (typeOfResponse === TargetGraphQLType.object) {
        if (Buffer.isBuffer(payload)) {
          try {
            responseBody = JSON.parse(payload.toString());
          } catch (e) {
            const errorString =
              `Cannot JSON parse payload` +
              `operation ${operation.operationString} ` +
              `even though it has content-type 'application/json'`;

            pubsubLog(errorString);
            return null;
          }
        } else {
          responseBody = payload;
        }
        saneData = Oas3Tools.sanitizeObjectKeys(payload);
      } else if (
        (Buffer.isBuffer(payload) || Array.isArray(payload)) &&
        typeOfResponse === TargetGraphQLType.string
      ) {
        saneData = payload.toString();
      }
    } else if (typeof payload === 'string') {
      if (typeOfResponse === TargetGraphQLType.object) {
        try {
          responseBody = JSON.parse(payload);
          saneData = Oas3Tools.sanitizeObjectKeys(responseBody);
        } catch (e) {
          const errorString =
            `Cannot JSON parse payload` +
            `operation ${operation.operationString} ` +
            `even though it has content-type 'application/json'`;

          pubsubLog(errorString);
          return null;
        }
      } else if (typeOfResponse === TargetGraphQLType.string) {
        saneData = payload;
      }
    }

    pubsubLog(
      `Message forwarded: ${JSON.stringify(saneData ? saneData : payload)}`
    );
    return saneData ? saneData : payload;
  };
}

/**
 * Returns values for link arguments, also covers the cases for
 * if the link parameter contains constants that are appended to the link parameter
 *
 * e.g. instead of:
 * $response.body#/employerId
 *
 * it could be:
 * abc_{$response.body#/employerId}
 */
function inferLinkArguments<TSource, TContext, TArgs>({
  paramName,
  value,
  resolveData,
  source,
  args,
}: inferLinkArgumentsParam<TSource, TContext, TArgs>) {
  if (typeof value === 'object') {
    return Object.entries(value).reduce((acc, [key, value]) => {
      acc[key] = inferLinkArguments({
        paramName,
        value,
        resolveData,
        source,
        args,
      });
      return acc;
    }, {});
  }

  if (typeof value !== 'string') {
    return value;
  } else if (value.search(/{|}/) === -1) {
    return isRuntimeExpression(value)
      ? resolveRuntimeExpression(paramName, value, resolveData, source, args)
      : value;
  } else {
    // Replace link parameters with appropriate values
    const linkParams = value.match(/{([^}]*)}/g);
    linkParams.forEach(linkParam => {
      value = value.replace(
        linkParam,
        resolveRuntimeExpression(
          paramName,
          linkParam.substring(1, linkParam.length - 1),
          resolveData,
          source,
          args
        )
      );
    });
    return value;
  }
}

/**
 * If the operation type is Query or Mutation, create and return a resolver
 * function that performs API requests for the given GraphQL query
 */
export function getResolver<TSource, TContext, TArgs extends object>({
  operation,
  argsFromLink = {},
  payloadName,
  data,
  baseUrl,
  requestOptions,
  fileUploadOptions,
  fetch,
}: GetResolverParams<TSource, TContext, TArgs>): GraphQLFieldResolver<
  TSource & OpenAPIToGraphQLSource<TSource, TContext, TArgs>,
  TContext,
  TArgs
> {
  // Determine the appropriate URL:
  if (typeof baseUrl === 'undefined') {
    baseUrl = Oas3Tools.getBaseUrl(operation);
  }

  // Return custom resolver if it is defined
  const customResolvers = data.options.customResolvers;
  const title = operation.oas.info.title;
  const path = operation.path;
  const method = operation.method;

  if (
    typeof customResolvers === 'object' &&
    typeof customResolvers[title] === 'object' &&
    typeof customResolvers[title][path] === 'object' &&
    typeof customResolvers[title][path][method] === 'function'
  ) {
    translationLog(`Use custom resolver for ${operation.operationString}`);

    return customResolvers[title][path][method];
  }

  // Return resolve function:
  return async (source, args, context, info) => {
    /**
     * Fetch resolveData from possibly existing _openAPIToGraphQL
     *
     * NOTE: _openAPIToGraphQL is an object used to pass security info and data
     * from previous resolvers
     */
    let resolveData: Partial<ResolveData<TSource, TContext, TArgs>> = {};
    if (
      source &&
      typeof source === 'object' &&
      typeof source[OPENAPI_TO_GRAPHQL] === 'object' &&
      typeof source[OPENAPI_TO_GRAPHQL].data === 'object'
    ) {
      const parentIdentifier = getParentIdentifier(info);
      if (
        !(parentIdentifier.length === 0) &&
        parentIdentifier in source[OPENAPI_TO_GRAPHQL].data
      ) {
        /**
         * Resolving link params may change the usedParams, but these changes
         * should not be present in the parent _openAPIToGraphQL, therefore copy
         * the object
         */
        resolveData = JSON.parse(
          JSON.stringify(source[OPENAPI_TO_GRAPHQL].data[parentIdentifier])
        );
      }
    }

    if (typeof resolveData.usedParams === 'undefined') {
      resolveData.usedParams = {};
    }

    /**
     * Handle default values of parameters, if they have not yet been defined by
     * the user.
     */
    operation.parameters.forEach(param => {
      const saneParamName = Oas3Tools.sanitize(
        param.name,
        !data.options.simpleNames
          ? Oas3Tools.CaseStyle.camelCase
          : Oas3Tools.CaseStyle.simple
      );
      if (
        typeof args[saneParamName] === 'undefined' &&
        param.schema &&
        typeof param.schema === 'object'
      ) {
        const schemaOrRef = param.schema;

        let schema: SchemaObject;
        if ('$ref' in schemaOrRef) {
          schema = Oas3Tools.resolveRef<SchemaObject>(
            schemaOrRef.$ref,
            operation.oas
          );
        } else {
          schema = schemaOrRef as SchemaObject;
        }

        if (schema && schema.default && typeof schema.default !== 'undefined') {
          args[saneParamName] = schema.default;
        }
      }
    });

    // Handle arguments provided by links
    for (const paramName in argsFromLink) {
      const saneParamName = Oas3Tools.sanitize(
        paramName,
        !data.options.simpleNames
          ? Oas3Tools.CaseStyle.camelCase
          : Oas3Tools.CaseStyle.simple
      );

      let value = argsFromLink[paramName];

      args[saneParamName] = inferLinkArguments({
        paramName,
        value,
        resolveData,
        source,
        args,
      });
    }

    // Stored used parameters to future requests:
    resolveData.usedParams = Object.assign(resolveData.usedParams, args);

    // Build URL (i.e., fill in path parameters):
    const { path, qs, headers } = extractRequestDataFromArgs(
      operation.path,
      operation.parameters,
      args,
      data
    );
    const url = new URL(urljoin(baseUrl, path));

    /**
     * The Content-Type and Accept property should not be changed because the
     * object type has already been created and unlike these properties, it
     * cannot be easily changed
     *
     * NOTE: This may cause the user to encounter unexpected changes
     */
    if (operation.method !== Oas3Tools.HTTP_METHODS.get) {
      headers['content-type'] =
        typeof operation.payloadContentType !== 'undefined'
          ? operation.payloadContentType
          : 'application/json';
    }

    headers['accept'] =
      typeof operation.responseContentType !== 'undefined'
        ? operation.responseContentType
        : 'application/json';

    let options: RequestInit;
    if (requestOptions) {
      options = {
        ...requestOptions,
        method: operation.method,
        headers: {},
      };

      options.headers = {}; // Handle requestOptions.header later if applicable

      if (requestOptions.headers) {
        // requestOptions.headers may be either an object or a function
        if (typeof requestOptions.headers === 'object') {
          Object.assign(options.headers, headers, requestOptions.headers);
        } else if (typeof requestOptions.headers === 'function') {
          const headers = requestOptions.headers(method, path, title, {
            source,
            args,
            context,
            info,
          });

          if (typeof headers === 'object') {
            Object.assign(options.headers, headers);
          }
        }
      } else {
        options.headers = headers;
      }

      if (typeof requestOptions.qs === 'object') {
        Object.assign(qs, requestOptions.qs);
      }
    } else {
      options = {
        method: operation.method,
        headers,
      };
    }

    /**
     * Determine possible payload
     *
     * GraphQL produces sanitized payload names, so we have to sanitize before
     * lookup here
     */
    let form: FormData;
    resolveData.usedPayload = undefined;
    if (typeof payloadName === 'string') {
      // The option genericPayloadArgName will change the payload name to "requestBody"
      const sanePayloadName = data.options.genericPayloadArgName
        ? 'requestBody'
        : Oas3Tools.sanitize(payloadName, Oas3Tools.CaseStyle.camelCase);

      let rawPayload;
      if (operation.payloadContentType === 'application/json') {
        rawPayload = JSON.stringify(
          Oas3Tools.desanitizeObjectKeys(args[sanePayloadName], data.saneMap)
        );
      } else if (
        operation.payloadContentType === 'application/x-www-form-urlencoded'
      ) {
        rawPayload = formurlencoded(
          Oas3Tools.desanitizeObjectKeys(args[sanePayloadName], data.saneMap)
        );
      } else if (operation.payloadContentType === 'multipart/form-data') {
        form = new FormData(fileUploadOptions);

        const formFieldsPayloadEntries = Object.entries(args[sanePayloadName]);

        (
          await Promise.all(formFieldsPayloadEntries.map(([_, v]) => v))
        ).forEach((fieldValue, idx) => {
          const fieldName = formFieldsPayloadEntries[idx][0];

          if (
            typeof fieldValue === 'object' &&
            Boolean((fieldValue as Partial<FileUpload>).createReadStream)
          ) {
            const uploadingFile = fieldValue as FileUpload;
            const originalFileStream = uploadingFile.createReadStream();
            const filePassThrough = new stream.PassThrough();

            originalFileStream.on('readable', function () {
              let data;

              while ((data = this.read())) {
                const canReadNext = filePassThrough.write(data);
                if (!canReadNext) {
                  this.pause();
                  filePassThrough.once('drain', () => this.resume());
                }
              }
            });

            originalFileStream.on('error', () => {
              uploadLog(
                'Encountered an error while uploading the file %s',
                uploadingFile.filename
              );
            });

            originalFileStream.on('end', () => {
              uploadLog(
                'Upload for received file %s completed',
                uploadingFile.filename
              );
              filePassThrough.end();
            });

            uploadLog(
              'Queuing upload for received file %s',
              uploadingFile.filename
            );

            form.append(fieldName, filePassThrough, {
              filename: uploadingFile.filename,
              contentType: uploadingFile.mimetype,
            });
          } else if (typeof fieldValue !== 'string') {
            // Handle all other primitives that aren't strings as strings the way the web server would expect it
            form.append(fieldName, JSON.stringify(fieldValue));
          } else {
            form.append(fieldName, fieldValue);
          }
        });

        rawPayload = form;
      } else {
        // Payload is not an object
        rawPayload = args[sanePayloadName];
      }
      options.body = rawPayload;
      resolveData.usedPayload = rawPayload;
    }

    /**
     * Pass on OpenAPI-to-GraphQL options
     */
    if (typeof data.options === 'object') {
      // Headers:
      if (typeof data.options.headers === 'object') {
        Object.assign(options.headers, data.options.headers);
      } else if (typeof data.options.headers === 'function') {
        const headers = data.options.headers(method, path, title, {
          source,
          args,
          context,
          info,
        });

        if (typeof headers === 'object') {
          Object.assign(options.headers, headers);
        }

        if (form) {
          /**
           * When there is a form, remove default content type and leave
           * computation of content-type header to fetch
           *
           * See https://github.com/github/fetch/issues/505#issuecomment-293064470
           */
          Object.assign(options.headers, form.getHeaders());
          delete options.headers['content-type'];
        }
      }

      // Query string:
      if (typeof data.options.qs === 'object') {
        Object.assign(qs, data.options.qs);
      }
    }

    // Get authentication headers and query parameters
    if (
      source &&
      typeof source === 'object' &&
      typeof source[OPENAPI_TO_GRAPHQL] === 'object'
    ) {
      const { authHeaders, authQs, authCookie } = getAuthOptions(
        operation,
        source[OPENAPI_TO_GRAPHQL],
        data
      );

      // ...and pass them to the options
      Object.assign(options.headers, authHeaders);
      Object.assign(qs, authQs);

      // Add authentication cookie if created
      if (authCookie !== null) {
        const cookieHeaderName = 'cookie';
        options.headers[cookieHeaderName] = authCookie;
      }
    }

    // Extract OAuth token from context (if available)
    if (data.options.sendOAuthTokenInQuery) {
      const oauthQueryObj = createOAuthQS(data, context);

      Object.assign(qs, oauthQueryObj);
    } else {
      const oauthHeader = createOAuthHeader(data, context);
      Object.assign(options.headers, oauthHeader);
    }

    resolveData.usedRequestOptions = options;
    resolveData.usedStatusCode = operation.statusCode;
    setSearchParamsFromObj(url, qs, []);
    resolveData.url = url.toString().replace(url.search, '');

    // Make the call
    httpLog(
      `Call ${options.method.toUpperCase()} ${url.toString()}\n` +
        `headers: ${JSON.stringify(options.headers)}\n` +
        `request body: ${options.body}`
    );

    let response: Response;
    try {
      response = await fetch(url.toString(), options);
    } catch (err) {
      httpLog(err);
      throw err;
    }

    const body = await response.text();
    if (response.status < 200 || response.status > 299) {
      httpLog(`${response.status} - ${Oas3Tools.trim(body, 100)}`);

      const errorString = `Could not invoke operation ${operation.operationString}`;

      if (data.options.provideErrorExtensions) {
        let responseBody;
        try {
          responseBody = JSON.parse(body);
        } catch (e) {
          responseBody = body;
        }

        const extensions = {
          method: operation.method,
          path: operation.path,
          url: url.toString(),
          statusText: response.statusText,
          statusCode: response.status,
          responseHeaders: headersToObject(response.headers),
          responseBody,
        };
        throw graphQLErrorWithExtensions(errorString, extensions);
      } else {
        throw new Error(errorString);
      }

      // Successful response code 200-299
    } else {
      httpLog(`${response.status} - ${Oas3Tools.trim(body, 100)}`);

      if (
        response.headers.get('content-type') &&
        operation.responseContentType
      ) {
        /**
         * Throw warning if the non-application/json content does not
         * match the OAS.
         *
         * Use an inclusion test in case of charset
         *
         * i.e. text/plain; charset=utf-8
         */
        if (
          !(
            response.headers
              .get('content-type')
              .includes(operation.responseContentType) ||
            operation.responseContentType.includes(
              response.headers.get('content-type')
            )
          )
        ) {
          const errorString =
            `Operation ` +
            `${operation.operationString} ` +
            `should have a content-type '${operation.responseContentType}' ` +
            `but has '${response.headers.get('content-type')}' instead`;

          httpLog(errorString);
          throw new Error(errorString);
        } else {
          /**
           * If the response body is type JSON, then parse it
           *
           * content-type may not be necessarily 'application/json' it can be
           * 'application/json; charset=utf-8' for example
           */
          if (
            response.headers.get('content-type').includes('application/json')
          ) {
            let responseBody;
            try {
              responseBody = JSON.parse(body);
            } catch (e) {
              const errorString =
                `Cannot JSON parse response body of ` +
                `operation ${operation.operationString} ` +
                `even though it has content-type 'application/json'`;

              httpLog(errorString);
              throw new Error(errorString);
            }

            resolveData.responseHeaders = {};
            response.headers.forEach((val, key) => {
              resolveData.responseHeaders[key] = val;
            });

            // Deal with the fact that the server might send unsanitized data
            let saneData = Oas3Tools.sanitizeObjectKeys(
              responseBody,
              !data.options.simpleNames
                ? Oas3Tools.CaseStyle.camelCase
                : Oas3Tools.CaseStyle.simple
            );

            // Pass on _openAPIToGraphQL to subsequent resolvers
            if (saneData && typeof saneData === 'object') {
              if (Array.isArray(saneData)) {
                saneData.forEach(element => {
                  if (typeof element[OPENAPI_TO_GRAPHQL] === 'undefined') {
                    element[OPENAPI_TO_GRAPHQL] = {
                      data: {},
                    };
                  }

                  if (
                    source &&
                    typeof source === 'object' &&
                    typeof source[OPENAPI_TO_GRAPHQL] === 'object'
                  ) {
                    Object.assign(
                      element[OPENAPI_TO_GRAPHQL],
                      source[OPENAPI_TO_GRAPHQL]
                    );
                  }

                  element[OPENAPI_TO_GRAPHQL].data[getIdentifier(info)] =
                    resolveData;
                });
              } else {
                if (typeof saneData[OPENAPI_TO_GRAPHQL] === 'undefined') {
                  saneData[OPENAPI_TO_GRAPHQL] = {
                    data: {},
                  };
                }

                if (
                  source &&
                  typeof source === 'object' &&
                  typeof source[OPENAPI_TO_GRAPHQL] === 'object'
                ) {
                  Object.assign(
                    saneData[OPENAPI_TO_GRAPHQL],
                    source[OPENAPI_TO_GRAPHQL]
                  );
                }

                saneData[OPENAPI_TO_GRAPHQL].data[getIdentifier(info)] =
                  resolveData;
              }
            }

            // Apply limit argument
            if (
              data.options.addLimitArgument &&
              /**
               * NOTE: Does not differentiate between autogenerated args and
               * preexisting args
               *
               * Ensure that there is not preexisting 'limit' argument
               */
              !operation.parameters.find(parameter => {
                return parameter.name === 'limit';
              }) &&
              // Only array data
              Array.isArray(saneData) &&
              // Only array of objects/arrays
              saneData.some(data => {
                return typeof data === 'object';
              })
            ) {
              let arraySaneData = saneData;

              if ('limit' in args && typeof args['limit'] === 'number') {
                const limit = args['limit'] as number;

                if (limit >= 0) {
                  arraySaneData = arraySaneData.slice(0, limit);
                } else {
                  throw new Error(
                    `Auto-generated 'limit' argument must be greater than or equal to 0`
                  );
                }
              } else {
                throw new Error(
                  `Cannot get value for auto-generated 'limit' argument`
                );
              }

              saneData = arraySaneData;
            }

            return saneData;
          } else {
            // TODO: Handle YAML

            return body;
          }
        }
      } else {
        /**
         * Check to see if there is not supposed to be a response body,
         * if that is the case, that would explain why there is not
         * a content-type
         */
        if (typeof operation.responseContentType !== 'string') {
          return null;
        } else {
          const errorString = 'Response does not have a Content-Type header';

          httpLog(errorString);
          throw new Error(errorString);
        }
      }
    }
  };
}

function headersToObject(headers: Headers) {
  const headersObj: HeadersInit = {};
  headers.forEach((value, key) => {
    headersObj[key] = value;
  });
  return headersObj;
}

/**
 * Attempts to create an object to become an OAuth query string by extracting an
 * OAuth token from the context based on the JSON path provided in the options.
 */
function createOAuthQS<TSource, TContext, TArgs>(
  data: PreprocessingData<TSource, TContext, TArgs>,
  context: TContext
): { [key: string]: string } {
  return typeof data.options.tokenJSONpath !== 'string'
    ? {}
    : extractToken(data, context);
}

function extractToken<TSource, TContext, TArgs>(
  data: PreprocessingData<TSource, TContext, TArgs>,
  context: TContext
) {
  const tokenJSONpath = data.options.tokenJSONpath;
  const tokens = JSONPath({
    path: tokenJSONpath,
    json: context as unknown as object,
  });
  if (Array.isArray(tokens) && tokens.length > 0) {
    const token = tokens[0];
    return {
      access_token: token,
    };
  } else {
    httpLog(
      `Warning: could not extract OAuth token from context at '${tokenJSONpath}'`
    );
    return {};
  }
}

/**
 * Attempts to create an OAuth authorization header by extracting an OAuth token
 * from the context based on the JSON path provided in the options.
 */
function createOAuthHeader<TSource, TContext, TArgs>(
  data: PreprocessingData<TSource, TContext, TArgs>,
  context: TContext
): { [key: string]: string } {
  if (typeof data.options.tokenJSONpath !== 'string') {
    return {};
  }

  // Extract token
  const tokenJSONpath = data.options.tokenJSONpath;
  const tokens = JSONPath({
    path: tokenJSONpath,
    json: context as unknown as object,
  });
  if (Array.isArray(tokens) && tokens.length > 0) {
    const token = tokens[0];
    return {
      Authorization: `Bearer ${token}`,
      'User-Agent': 'openapi-to-graphql',
    };
  } else {
    httpLog(
      `Warning: could not extract OAuth token from context at ` +
        `'${tokenJSONpath}'`
    );
    return {};
  }
}

/**
 * Return the headers and query strings to authenticate a request (if any).
 * Return authHeader and authQs, which hold headers and query parameters
 * respectively to authentication a request.
 */
function getAuthOptions<TSource, TContext, TArgs>(
  operation: Operation,
  _openAPIToGraphQL: OpenAPIToGraphQLRoot<TSource, TContext, TArgs>,
  data: PreprocessingData<TSource, TContext, TArgs>
): AuthOptions {
  const authHeaders = {};
  const authQs = {};
  let authCookie = null;

  /**
   * Determine if authentication is required, and which protocol (if any) we can
   * use
   */
  const { authRequired, securityRequirement, sanitizedSecurityRequirement } =
    getAuthReqAndProtcolName(operation, _openAPIToGraphQL);

  // Possibly, we don't need to do anything:
  if (!authRequired) {
    return { authHeaders, authQs, authCookie };
  }

  // If authentication is required, but we can't fulfill the protocol, throw:
  if (authRequired && typeof securityRequirement !== 'string') {
    throw new Error(`Missing information to authenticate API request.`);
  }

  if (typeof securityRequirement === 'string') {
    const security = data.security[securityRequirement];
    switch (security.def.type) {
      case 'apiKey':
        const apiKey =
          _openAPIToGraphQL.security[sanitizedSecurityRequirement].apiKey;
        if ('in' in security.def) {
          if (typeof security.def.name === 'string') {
            if (security.def.in === 'header') {
              authHeaders[security.def.name] = apiKey;
            } else if (security.def.in === 'query') {
              authQs[security.def.name] = apiKey;
            } else if (security.def.in === 'cookie') {
              authCookie = `${security.def.name}=${apiKey}`;
            }
          } else {
            throw new Error(
              `Cannot send API key in '${JSON.stringify(security.def.in)}'`
            );
          }
        }
        break;

      case 'http':
        switch (security.def.scheme) {
          case 'basic':
            const username =
              _openAPIToGraphQL.security[sanitizedSecurityRequirement].username;
            const password =
              _openAPIToGraphQL.security[sanitizedSecurityRequirement].password;
            const credentials = `${username}:${password}`;
            authHeaders['Authorization'] = `Basic ${Buffer.from(
              credentials
            ).toString('base64')}`;
            break;
          case 'bearer':
            const token =
              _openAPIToGraphQL.security[sanitizedSecurityRequirement].token;
            authHeaders['Authorization'] = `Bearer ${token}`;
            break;
          default:
            throw new Error(
              `Cannot recognize http security scheme ` +
                `'${JSON.stringify(security.def.scheme)}'`
            );
        }
        break;

      case 'oauth2':
        break;

      case 'openIdConnect':
        break;

      default:
        throw new Error(
          `Cannot recognize security type '${security.def.type}'`
        );
    }
  }
  return { authHeaders, authQs, authCookie };
}

/**
 * Determines whether a given operation requires authentication, and which of
 * the (possibly multiple) authentication protocols can be used based on the
 * data present in the given context.
 */
function getAuthReqAndProtcolName<TSource, TContext, TArgs>(
  operation: Operation,
  _openAPIToGraphQL: OpenAPIToGraphQLRoot<TSource, TContext, TArgs>
): AuthReqAndProtcolName {
  let authRequired = false;
  if (
    Array.isArray(operation.securityRequirements) &&
    operation.securityRequirements.length > 0
  ) {
    authRequired = true;

    for (let securityRequirement of operation.securityRequirements) {
      const sanitizedSecurityRequirement = Oas3Tools.sanitize(
        securityRequirement,
        Oas3Tools.CaseStyle.camelCase
      );
      if (
        typeof _openAPIToGraphQL.security[sanitizedSecurityRequirement] ===
        'object'
      ) {
        return {
          authRequired,
          securityRequirement,
          sanitizedSecurityRequirement,
        };
      }
    }
  }
  return {
    authRequired,
  };
}

/**
 * Given a link parameter or callback path, determine the value from the runtime
 * expression
 *
 * The link parameter or callback path is a reference to data contained in the
 * url/method/statuscode or response/request body/query/path/header
 */
function resolveRuntimeExpression(
  paramName: string,
  runtimeExpression: string,
  resolveData: any,
  root: any,
  args: any
): any {
  if (runtimeExpression === '$url') {
    return resolveData.url;
  } else if (runtimeExpression === '$method') {
    return resolveData.usedRequestOptions.method;
  } else if (runtimeExpression === '$statusCode') {
    return resolveData.usedStatusCode;
  } else if (runtimeExpression.startsWith('$request.')) {
    // CASE: parameter is previous body
    if (runtimeExpression === '$request.body') {
      return resolveData.usedPayload;

      // CASE: parameter in previous body
    } else if (runtimeExpression.startsWith('$request.body#')) {
      const tokens = JSONPath({
        path: runtimeExpression.split('body#/')[1],
        json: resolveData.usedPayload,
      });
      if (Array.isArray(tokens) && tokens.length > 0) {
        return tokens[0];
      } else {
        httpLog(
          `Warning: could not extract parameter '${paramName}' from link`
        );
      }

      // CASE: parameter in previous query parameter
    } else if (runtimeExpression.startsWith('$request.query')) {
      return resolveData.usedParams[
        Oas3Tools.sanitize(
          runtimeExpression.split('query.')[1],
          Oas3Tools.CaseStyle.camelCase
        )
      ];

      // CASE: parameter in previous path parameter
    } else if (runtimeExpression.startsWith('$request.path')) {
      return resolveData.usedParams[
        Oas3Tools.sanitize(
          runtimeExpression.split('path.')[1],
          Oas3Tools.CaseStyle.camelCase
        )
      ];

      // CASE: parameter in previous header parameter
    } else if (runtimeExpression.startsWith('$request.header')) {
      return resolveData.usedRequestOptions.headers[
        runtimeExpression.split('header.')[1]
      ];
    }
  } else if (runtimeExpression.startsWith('$response.')) {
    /**
     * CASE: parameter is body
     *
     * NOTE: may not be used because it implies that the operation does not
     * return a JSON object and OpenAPI-to-GraphQL does not create GraphQL
     * objects for non-JSON data and links can only exists between objects.
     */
    if (runtimeExpression === '$response.body') {
      const result = JSON.parse(JSON.stringify(root));
      /**
       * _openAPIToGraphQL contains data used by OpenAPI-to-GraphQL to create the GraphQL interface
       * and should not be exposed
       */
      result._openAPIToGraphQL = undefined;
      return result;

      // CASE: parameter in body
    } else if (runtimeExpression.startsWith('$response.body#')) {
      return JSONPointer.get(root, runtimeExpression.split('body#')[1]);

      // CASE: parameter in query parameter
    } else if (runtimeExpression.startsWith('$response.query')) {
      // NOTE: handled the same way $request.query is handled
      return resolveData.usedParams[
        Oas3Tools.sanitize(
          runtimeExpression.split('query.')[1],
          Oas3Tools.CaseStyle.camelCase
        )
      ];

      // CASE: parameter in path parameter
    } else if (runtimeExpression.startsWith('$response.path')) {
      // NOTE: handled the same way $request.path is handled
      return resolveData.usedParams[
        Oas3Tools.sanitize(
          runtimeExpression.split('path.')[1],
          Oas3Tools.CaseStyle.camelCase
        )
      ];

      // CASE: parameter in header parameter
    } else if (runtimeExpression.startsWith('$response.header')) {
      return resolveData.responseHeaders[runtimeExpression.split('header.')[1]];
    }
  }

  throw new Error(
    `Cannot resolve link because '${runtimeExpression}' is an invalid runtime expression.`
  );
}

/**
 * Check if a string is a runtime expression in the context of link parameters
 */
function isRuntimeExpression(str: string): boolean {
  if (str === '$url' || str === '$method' || str === '$statusCode') {
    return true;
  } else if (str.startsWith('$request.')) {
    for (let i = 0; i < RUNTIME_REFERENCES.length; i++) {
      if (str.startsWith(`$request.${RUNTIME_REFERENCES[i]}`)) {
        return true;
      }
    }
  } else if (str.startsWith('$response.')) {
    for (let i = 0; i < RUNTIME_REFERENCES.length; i++) {
      if (str.startsWith(`$response.${RUNTIME_REFERENCES[i]}`)) {
        return true;
      }
    }
  }

  return false;
}

/**
 * From the info object provided by the resolver, get a unique identifier, which
 * is the path formed from the nested field names (or aliases if provided)
 *
 * Used to store and retrieve the _openAPIToGraphQL of parent field
 */
function getIdentifier(info): string {
  return getIdentifierRecursive(info.path);
}

/**
 * From the info object provided by the resolver, get the unique identifier of
 * the parent object
 */
function getParentIdentifier(info): string {
  return getIdentifierRecursive(info.path.prev);
}

/**
 * Get the path of nested field names (or aliases if provided)
 */
function getIdentifierRecursive(path): string {
  return typeof path.prev === 'undefined'
    ? path.key
    : /**
     * Check if the identifier contains array indexing, if so remove.
     *
     * i.e. instead of 0/friends/1/friends/2/friends/user, create
     * friends/friends/friends/user
     */
    isNaN(parseInt(path.key))
    ? `${path.key}/${getIdentifierRecursive(path.prev)}`
    : getIdentifierRecursive(path.prev);
}

/**
 * Create a new GraphQLError with an extensions field
 */
function graphQLErrorWithExtensions(
  message: string,
  extensions: { [key: string]: any }
): GraphQLError {
  return new GraphQLError(message, null, null, null, null, null, extensions);
}

/**
 * Extracts data from the GraphQL arguments of a particular field
 *
 * Replaces the path parameter in the given path with values in the given args.
 * Furthermore adds the query parameters for a request.
 */
export function extractRequestDataFromArgs<
  TSource,
  TContext,
  TArgs extends object
>(
  path: string,
  parameters: ParameterObject[],
  args: TArgs, // NOTE: argument keys are sanitized!
  data: PreprocessingData<TSource, TContext, TArgs>
): {
  path: string;
  qs: { [key: string]: string };
  headers: { [key: string]: string };
} {
  const qs = {};
  const headers = {};

  // Iterate parameters:
  for (const param of parameters) {
    const saneParamName = Oas3Tools.sanitize(
      param.name,
      !data.options.simpleNames
        ? Oas3Tools.CaseStyle.camelCase
        : Oas3Tools.CaseStyle.simple
    );

    if (saneParamName && saneParamName in args) {
      switch (param.in) {
        // Path parameters
        case 'path':
          path = path.replace(`{${param.name}}`, args[saneParamName]);
          break;

        // Query parameters
        case 'query':
          // setting param style as form assumes explode is true by default
          if (
            param.style === 'form' &&
            typeof args[saneParamName] === 'object'
          ) {
            if (param.explode === false) {
              qs[param.name] = Object.entries(args[saneParamName]).reduce(
                (acc, val) => {
                  acc += val.join(',');
                  return acc;
                },
                ''
              );
            } else {
              Object.entries(args[saneParamName]).forEach(([key, value]) => {
                qs[key] = value;
              });
            }
          } else if (
            Array.isArray(args[saneParamName]) &&
            param.style === 'form' &&
            param.explode !== false
          ) {
            qs[param.name] = args[saneParamName].join(',');
          } else {
            qs[param.name] = args[saneParamName];
          }
          break;

        // Header parameters
        case 'header':
          headers[param.name] = args[saneParamName];
          break;

        // Cookie parameters
        case 'cookie':
          if (!('cookie' in headers)) {
            headers['cookie'] = '';
          }

          headers['cookie'] += `${param.name}=${args[saneParamName]}; `;
          break;

        default:
          httpLog(
            `Warning: The parameter location '${param.in}' in the ` +
              `parameter '${param.name}' of operation '${path}' is not ` +
              `supported`
          );
      }
    }
  }

  return { path, qs, headers };
}

const setSearchParamsFromObj = (url: URL, obj: any, path: string[]) => {
  for (const key in obj) {
    const val = obj[key];
    const newPath = [...path, key];
    if (typeof val === 'object') {
      setSearchParamsFromObj(url, val, newPath);
    } else {
      const finalKey = newPath.reduce(
        (acc, pathElem, i) => (i === 0 ? pathElem : `${acc}[${pathElem}]`),
        ''
      );
      url.searchParams.set(finalKey, val);
    }
  }
};
