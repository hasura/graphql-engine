// Copyright IBM Corp. 2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

// Type imports:
import {
  Oas3,
  CallbackObject,
  LinkObject,
  OperationObject,
  ReferenceObject,
  SchemaObject,
  PathItemObject,
} from './types/oas3';
import { InternalOptions } from './types/options';
import {
  Operation,
  DataDefinition,
  TargetGraphQLType,
} from './types/operation';
import {
  PreprocessingData,
  ProcessedSecurityScheme,
} from './types/preprocessing_data';

// Imports:
import * as Oas3Tools from './oas_3_tools';
import deepEqual from 'deep-equal';
import debug from 'debug';
import {
  handleWarning,
  getCommonPropertyNames,
  MitigationTypes,
} from './utils';
import { GraphQLOperationType } from './types/graphql';
import { methodToHttpMethod } from './oas_3_tools';
import OpenAPIParser from '@readme/openapi-parser';

const preprocessingLog = debug('preprocessing');

/**
 * Given an operation object from the OAS, create an Operation, which contains
 * the necessary data to create a GraphQL wrapper for said operation object.
 *
 * @param path The path of the operation object
 * @param method The method of the operation object
 * @param operationString A string representation of the path and the method (and the OAS title if applicable)
 * @param operationType Whether the operation should be turned into a Query/Mutation/Subscription operation
 * @param operation The operation object from the OAS
 * @param pathItem The path item object from the OAS from which the operation object is derived from
 * @param oas The OAS from which the path item and operation object are derived from
 * @param data An assortment of data which at this point is mainly used enable logging
 * @param options The options passed by the user
 */
function processOperation<TSource, TContext, TArgs>(
  path: string,
  method: Oas3Tools.HTTP_METHODS,
  operationString: string,
  operationType: GraphQLOperationType,
  operation: OperationObject,
  pathItem: PathItemObject,
  oas: Oas3,
  data: PreprocessingData<TSource, TContext, TArgs>,
  options: InternalOptions<TSource, TContext, TArgs>
): Operation {
  // Response schema
  const {
    responseContentType,
    responseSchema,
    responseSchemaNames,
    statusCode,
  } = Oas3Tools.getResponseSchemaAndNames(
    path,
    method,
    operation,
    oas,
    data,
    options
  );

  /**
   * All GraphQL fields must have a type, which is derived from the response
   * schema. Therefore, the response schema is the first to be determined.
   */
  if (typeof responseSchema === 'object') {
    // Description
    let description = operation.description;
    if (
      (typeof description !== 'string' || description === '') &&
      typeof operation.summary === 'string'
    ) {
      description = operation.summary;
    }

    if (data.options.equivalentToMessages) {
      // Description may not exist
      if (typeof description !== 'string') {
        description = `Equivalent to ${operationString}`;
      } else {
        description += `\n\nEquivalent to ${operationString}`;
      }
    }

    // Tags
    const tags = operation.tags || [];

    // OperationId
    const operationId =
      typeof operation.operationId !== 'undefined'
        ? operation.operationId
        : Oas3Tools.generateOperationId(method, path);

    // Request schema
    const {
      payloadContentType,
      payloadSchema,
      payloadSchemaNames,
      payloadRequired,
    } = Oas3Tools.getRequestSchemaAndNames(path, method, operation, oas);

    // Request data definition
    const payloadDefinition =
      payloadSchema && typeof payloadSchema !== 'undefined'
        ? createDataDef(
            payloadSchemaNames,
            payloadSchema as SchemaObject,
            true,
            data,
            oas
          )
        : undefined;

    // Links
    const links = Oas3Tools.getLinks(path, method, operation, oas, data);

    // Response data definition
    const responseDefinition = createDataDef(
      responseSchemaNames,
      responseSchema as SchemaObject,
      false,
      data,
      oas,
      links
    );

    // Parameters
    const parameters = Oas3Tools.getParameters(
      path,
      method,
      operation,
      pathItem,
      oas
    );

    // Security protocols
    const securityRequirements = options.viewer
      ? Oas3Tools.getSecurityRequirements(operation, data.security, oas)
      : [];

    // Servers
    const servers = Oas3Tools.getServers(operation, pathItem, oas);

    // Whether to place this operation into an authentication viewer
    const inViewer =
      securityRequirements.length > 0 && data.options.viewer !== false;

    return {
      operation,
      operationId,
      operationString,
      operationType,
      description,
      tags,
      path,
      method,
      payloadContentType,
      payloadDefinition,
      payloadRequired,
      responseContentType,
      responseDefinition,
      parameters,
      securityRequirements,
      servers,
      inViewer,
      statusCode,
      oas,
    };
  } else {
    handleWarning({
      mitigationType: MitigationTypes.MISSING_RESPONSE_SCHEMA,
      message:
        `Operation ${operationString} has no (valid) response schema. ` +
        `You can use the fillEmptyResponses option to create a ` +
        `placeholder schema`,
      data,
      log: preprocessingLog,
    });
  }
}

/**
 * Extract information from the OAS and put it inside a data structure that
 * is easier for OpenAPI-to-GraphQL to use
 */
export async function preprocessOas<TSource, TContext, TArgs>(
  oass: Oas3[],
  options: InternalOptions<TSource, TContext, TArgs>
): Promise<PreprocessingData<TSource, TContext, TArgs>> {
  const data: PreprocessingData<TSource, TContext, TArgs> = {
    operations: {},
    callbackOperations: {},
    usedTypeNames: [
      'Query', // Used by OpenAPI-to-GraphQL for root-level element
      'Mutation', // Used by OpenAPI-to-GraphQL for root-level element
      'Subscription', // Used by OpenAPI-to-GraphQL for root-level element
    ],
    defs: [],
    security: {},
    saneMap: {},
    options,
    oass,
  };

  await Promise.all(
    oass.map(async oas => {
      // Store stats on OAS:
      try {
        await OpenAPIParser.validate(
          JSON.parse(JSON.stringify(oas)),
          options.oasValidatorOptions
        );
      } catch (e) {
        data.options.report.validationErrors = (
          data.options.report.validationErrors || []
        ).concat(e.message);
      }
      data.options.report.numOps += Oas3Tools.countOperations(oas);
      data.options.report.numOpsMutation +=
        Oas3Tools.countOperationsMutation(oas);
      data.options.report.numOpsQuery += Oas3Tools.countOperationsQuery(oas);
      if (data.options.createSubscriptionsFromCallbacks) {
        data.options.report.numOpsSubscription +=
          Oas3Tools.countOperationsSubscription(oas);
      } else {
        data.options.report.numOpsSubscription = 0;
      }

      // Get security schemes
      const currentSecurity = getProcessedSecuritySchemes(oas, data);
      const commonSecurityPropertyName = getCommonPropertyNames(
        data.security,
        currentSecurity
      );
      commonSecurityPropertyName.forEach(propertyName => {
        handleWarning({
          mitigationType: MitigationTypes.DUPLICATE_SECURITY_SCHEME,
          message: `Multiple OASs share security schemes with the same name '${propertyName}'`,
          mitigationAddendum:
            `The security scheme from OAS ` +
            `'${currentSecurity[propertyName].oas.info.title}' will be ignored`,
          data,
          log: preprocessingLog,
        });
      });

      // Do not overwrite preexisting security schemes
      data.security = { ...currentSecurity, ...data.security };

      // Process all operations
      for (let path in oas.paths) {
        const pathItem =
          typeof oas.paths[path].$ref === 'string'
            ? (Oas3Tools.resolveRef(
                oas.paths[path].$ref,
                oas
              ) as PathItemObject)
            : oas.paths[path];

        Object.keys(pathItem)
          .filter(pathFields => {
            /**
             * Get only method fields that contain operation objects (e.g. "get",
             * "put", "post", "delete", etc.)
             *
             * Can also contain other fields such as summary or description
             */
            return Oas3Tools.isHttpMethod(pathFields);
          })
          .forEach(rawMethod => {
            const operationString =
              oass.length === 1
                ? Oas3Tools.formatOperationString(rawMethod, path)
                : Oas3Tools.formatOperationString(
                    rawMethod,
                    path,
                    oas.info.title
                  );

            let httpMethod: Oas3Tools.HTTP_METHODS;
            try {
              httpMethod = methodToHttpMethod(rawMethod);
            } catch (e) {
              handleWarning({
                mitigationType: MitigationTypes.INVALID_HTTP_METHOD,
                message: `Invalid HTTP method '${rawMethod}' in operation '${operationString}'`,
                data,
                log: preprocessingLog,
              });

              return;
            }

            const operation = pathItem[httpMethod] as OperationObject;

            let operationType =
              httpMethod === Oas3Tools.HTTP_METHODS.get
                ? GraphQLOperationType.Query
                : GraphQLOperationType.Mutation;

            // Option selectQueryOrMutationField can override operation type
            if (
              typeof options?.selectQueryOrMutationField?.[oas.info.title]?.[
                path
              ]?.[httpMethod] === 'number'
              // This is an enum, which is an integer value
            ) {
              operationType =
                options.selectQueryOrMutationField[oas.info.title][path][
                  httpMethod
                ] === GraphQLOperationType.Mutation
                  ? GraphQLOperationType.Mutation
                  : GraphQLOperationType.Query;
            }

            const operationData = processOperation(
              path,
              httpMethod,
              operationString,
              operationType,
              operation,
              pathItem,
              oas,
              data,
              options
            );

            if (typeof operationData === 'object') {
              /**
               * Handle operationId property name collision
               * May occur if multiple OAS are provided
               */
              if (!(operationData.operationId in data.operations)) {
                data.operations[operationData.operationId] = operationData;
              } else {
                handleWarning({
                  mitigationType: MitigationTypes.DUPLICATE_OPERATIONID,
                  message: `Multiple OASs share operations with the same operationId '${operationData.operationId}'`,
                  mitigationAddendum: `The operation from the OAS '${operationData.oas.info.title}' will be ignored`,
                  data,
                  log: preprocessingLog,
                });

                return;
              }
            }

            // Process all callbacks
            if (
              data.options.createSubscriptionsFromCallbacks &&
              operation.callbacks
            ) {
              Object.entries(operation.callbacks).forEach(
                ([callbackName, callbackObjectOrRef]) => {
                  let callback: CallbackObject;

                  if (
                    '$ref' in callbackObjectOrRef &&
                    typeof callbackObjectOrRef.$ref === 'string'
                  ) {
                    callback = Oas3Tools.resolveRef(
                      callbackObjectOrRef.$ref,
                      oas
                    );
                  } else {
                    callback = callbackObjectOrRef as CallbackObject;
                  }

                  Object.entries(callback).forEach(
                    ([callbackExpression, callbackPathItem]) => {
                      const resolvedCallbackPathItem = !(
                        '$ref' in callbackPathItem
                      )
                        ? callbackPathItem
                        : Oas3Tools.resolveRef(callbackPathItem.$ref, oas);

                      const callbackOperationObjectMethods = Object.keys(
                        resolvedCallbackPathItem
                      ).filter(objectKey => {
                        /**
                         * Get only fields that contain operation objects
                         *
                         * Can also contain other fields such as summary or description
                         */
                        return Oas3Tools.isHttpMethod(objectKey);
                      });

                      if (callbackOperationObjectMethods.length > 0) {
                        if (callbackOperationObjectMethods.length > 1) {
                          handleWarning({
                            mitigationType:
                              MitigationTypes.CALLBACKS_MULTIPLE_OPERATION_OBJECTS,
                            message: `Callback '${callbackExpression}' on operation '${operationString}' has multiple operation objects with the methods '${callbackOperationObjectMethods}'. OpenAPI-to-GraphQL can only utilize one of these operation objects.`,
                            mitigationAddendum: `The operation with the method '${callbackOperationObjectMethods[0]}' will be selected and all others will be ignored.`,
                            data,
                            log: preprocessingLog,
                          });
                        }

                        // Select only one of the operation object methods
                        const callbackRawMethod =
                          callbackOperationObjectMethods[0];

                        const callbackOperationString =
                          oass.length === 1
                            ? Oas3Tools.formatOperationString(
                                httpMethod,
                                callbackName
                              )
                            : Oas3Tools.formatOperationString(
                                httpMethod,
                                callbackName,
                                oas.info.title
                              );

                        let callbackHttpMethod: Oas3Tools.HTTP_METHODS;

                        try {
                          callbackHttpMethod =
                            methodToHttpMethod(callbackRawMethod);
                        } catch (e) {
                          handleWarning({
                            mitigationType: MitigationTypes.INVALID_HTTP_METHOD,
                            message: `Invalid HTTP method '${rawMethod}' in callback '${callbackOperationString}' in operation '${operationString}'`,
                            data,
                            log: preprocessingLog,
                          });

                          return;
                        }

                        const callbackOperation = processOperation(
                          callbackExpression,
                          callbackHttpMethod,
                          callbackOperationString,
                          GraphQLOperationType.Subscription,
                          resolvedCallbackPathItem[callbackHttpMethod],
                          callbackPathItem,
                          oas,
                          data,
                          options
                        );

                        if (callbackOperation) {
                          /**
                           * Handle operationId property name collision
                           * May occur if multiple OAS are provided
                           */
                          if (
                            callbackOperation &&
                            !(
                              callbackOperation.operationId in
                              data.callbackOperations
                            )
                          ) {
                            data.callbackOperations[
                              callbackOperation.operationId
                            ] = callbackOperation;
                          } else {
                            handleWarning({
                              mitigationType:
                                MitigationTypes.DUPLICATE_OPERATIONID,
                              message: `Multiple OASs share callback operations with the same operationId '${callbackOperation.operationId}'`,
                              mitigationAddendum: `The callback operation from the OAS '${operationData.oas.info.title}' will be ignored`,
                              data,
                              log: preprocessingLog,
                            });
                          }
                        }
                      }
                    }
                  );
                }
              );
            }
          });
      }
    })
  );

  return data;
}

/**
 * Extracts the security schemes from given OAS and organizes the information in
 * a data structure that is easier for OpenAPI-to-GraphQL to use
 *
 * Here is the structure of the data:
 * {
 *   {string} [sanitized name] { Contains information about the security protocol
 *     {string} rawName           Stores the raw security protocol name
 *     {object} def               Definition provided by OAS
 *     {object} parameters        Stores the names of the authentication credentials
 *                                  NOTE: Structure will depend on the type of the protocol
 *                                    (e.g. basic authentication, API key, etc.)
 *                                  NOTE: Mainly used for the AnyAuth viewers
 *     {object} schema            Stores the GraphQL schema to create the viewers
 *   }
 * }
 *
 * Here is an example:
 * {
 *   MyApiKey: {
 *     rawName: "My_api_key",
 *     def: { ... },
 *     parameters: {
 *       apiKey: MyKeyApiKey
 *     },
 *     schema: { ... }
 *   }
 *   MyBasicAuth: {
 *     rawName: "My_basic_auth",
 *     def: { ... },
 *     parameters: {
 *       username: MyBasicAuthUsername,
 *       password: MyBasicAuthPassword,
 *     },
 *     schema: { ... }
 *   }
 * }
 */
function getProcessedSecuritySchemes<TSource, TContext, TArgs>(
  oas: Oas3,
  data: PreprocessingData<TSource, TContext, TArgs>
): { [key: string]: ProcessedSecurityScheme } {
  const result = {};
  const security = Oas3Tools.getSecuritySchemes(oas);

  // Loop through all the security protocols
  for (let schemeKey in security) {
    const securityScheme = security[schemeKey];

    // Determine the schema and the parameters for the security protocol
    let schema;
    let parameters = {};
    let description;
    switch (securityScheme.type) {
      case 'apiKey':
        description = `API key credentials for the security protocol '${schemeKey}'`;
        if (data.oass.length > 1) {
          description += ` in ${oas.info.title}`;
        }

        parameters = {
          apiKey: Oas3Tools.sanitize(
            `${schemeKey}_apiKey`,
            Oas3Tools.CaseStyle.camelCase
          ),
        };

        schema = {
          type: 'object',
          description,
          properties: {
            apiKey: {
              type: 'string',
            },
          },
        };
        break;

      case 'http':
        switch (securityScheme.scheme) {
          /**
           * TODO: HTTP has a number of authentication types
           *
           * See http://www.iana.org/assignments/http-authschemes/http-authschemes.xhtml
           */
          case 'basic':
            description = `Basic auth credentials for security protocol '${schemeKey}'`;

            parameters = {
              username: Oas3Tools.sanitize(
                `${schemeKey}_username`,
                Oas3Tools.CaseStyle.camelCase
              ),
              password: Oas3Tools.sanitize(
                `${schemeKey}_password`,
                Oas3Tools.CaseStyle.camelCase
              ),
            };

            schema = {
              type: 'object',
              description,
              properties: {
                username: {
                  type: 'string',
                },
                password: {
                  type: 'string',
                },
              },
            };
            break;

          case 'bearer':
            description = `Bearer auth credentials for security protocol '${schemeKey}'`;

            parameters = {
              token: Oas3Tools.sanitize(
                `${schemeKey}_token`,
                Oas3Tools.CaseStyle.camelCase
              ),
            };

            schema = {
              type: 'object',
              description,
              properties: {
                token: {
                  type: 'string',
                },
              },
            };
            break;

          default:
            handleWarning({
              mitigationType: MitigationTypes.UNSUPPORTED_HTTP_SECURITY_SCHEME,
              message:
                `Currently unsupported HTTP authentication protocol ` +
                `type 'http' and scheme '${securityScheme.scheme}' in OAS ` +
                `'${oas.info.title}'`,
              data,
              log: preprocessingLog,
            });
        }
        break;

      // TODO: Implement
      case 'openIdConnect':
        handleWarning({
          mitigationType: MitigationTypes.UNSUPPORTED_HTTP_SECURITY_SCHEME,
          message:
            `Currently unsupported HTTP authentication protocol ` +
            `type 'openIdConnect' in OAS '${oas.info.title}'`,
          data,
          log: preprocessingLog,
        });

        break;

      case 'oauth2':
        handleWarning({
          mitigationType: MitigationTypes.OAUTH_SECURITY_SCHEME,
          message: `OAuth security scheme found in OAS '${oas.info.title}'`,
          data,
          log: preprocessingLog,
        });

        // Continue because we do not want to create an OAuth viewer
        continue;

      default:
        handleWarning({
          mitigationType: MitigationTypes.UNSUPPORTED_HTTP_SECURITY_SCHEME,
          message:
            `Unsupported HTTP authentication protocol` +
            `type '${securityScheme.type}' in OAS '${oas.info.title}'`,
          data,
          log: preprocessingLog,
        });
    }

    // Add protocol data to the output
    result[schemeKey] = {
      rawName: schemeKey,
      def: securityScheme,
      parameters,
      schema,
      oas,
    };
  }
  return result;
}

/**
 * Method to either create a new or reuse an existing, centrally stored data
 * definition.
 */
export function createDataDef<TSource, TContext, TArgs>(
  names: Oas3Tools.SchemaNames,
  schemaOrRef: SchemaObject | ReferenceObject,
  isInputObjectType: boolean,
  data: PreprocessingData<TSource, TContext, TArgs>,
  oas: Oas3,
  links?: { [key: string]: LinkObject }
): DataDefinition {
  const preferredName = getPreferredName(names);

  // Basic validation test
  if (typeof schemaOrRef !== 'object' && schemaOrRef !== null) {
    handleWarning({
      mitigationType: MitigationTypes.MISSING_SCHEMA,
      message:
        `Could not create data definition for schema with ` +
        `preferred name '${preferredName}' and schema '${JSON.stringify(
          schemaOrRef
        )}'`,
      data,
      log: preprocessingLog,
    });

    return {
      preferredName,
      schema: null,
      required: [],
      links: null,
      subDefinitions: null,
      graphQLTypeName: null,
      graphQLInputObjectTypeName: null,
      targetGraphQLType: TargetGraphQLType.json,
    };
  }

  let schema: SchemaObject;
  if ('$ref' in schemaOrRef && typeof schemaOrRef.$ref === 'string') {
    schema = Oas3Tools.resolveRef(schemaOrRef.$ref, oas);
  } else {
    schema = schemaOrRef as SchemaObject;
  }

  // Sanitize link keys
  const saneLinks = sanitizeLinks({ links, data });

  // Check for preexisting data definition
  const index = getSchemaIndex(preferredName, schema, data.defs);
  if (index !== -1) {
    // Found existing data definition and fetch it
    const existingDataDef = data.defs[index];

    /**
     * Special handling for oneOf. Subdefinitions are always an array
     * (see createOneOfUnion)
     */
    if (
      existingDataDef.targetGraphQLType === TargetGraphQLType.oneOfUnion &&
      Array.isArray(existingDataDef.subDefinitions)
    ) {
      existingDataDef.subDefinitions.forEach(def => {
        collapseLinksIntoDataDefinition({
          additionalLinks: saneLinks,
          existingDataDef: def,
          data,
        });
      });
    } else {
      collapseLinksIntoDataDefinition({
        additionalLinks: saneLinks,
        existingDataDef,
        data,
      });
    }

    return existingDataDef;
  }

  // There is no preexisting data definition, so create a new one

  const name = getSchemaName(names, data.usedTypeNames);

  let saneInputName: string;
  let saneName: string;

  if (name === names.fromExtension) {
    saneName = name;
    saneInputName = name + 'Input';
  } else {
    // Store and sanitize the name
    saneName = !data.options.simpleNames
      ? Oas3Tools.sanitize(name, Oas3Tools.CaseStyle.PascalCase)
      : Oas3Tools.capitalize(
          Oas3Tools.sanitize(name, Oas3Tools.CaseStyle.simple)
        );
    saneInputName = Oas3Tools.capitalize(saneName + 'Input');
  }

  Oas3Tools.storeSaneName(saneName, name, data.saneMap);

  /**
   * Recursively resolve allOf so type, properties, anyOf, oneOf, and
   * required are resolved
   */
  let collapsedSchema = Oas3Tools.resolveAnyOf(schema) as SchemaObject;

  collapsedSchema = Oas3Tools.resolveAllOf(
    collapsedSchema,
    {},
    data,
    oas
  ) as SchemaObject;

  const targetGraphQLType = Oas3Tools.getSchemaTargetGraphQLType(
    collapsedSchema,
    data,
    oas
  );

  const def: DataDefinition = {
    preferredName,

    /**
     * Note that schema may contain $ref or schema composition (e.g. allOf)
     *
     * TODO: the schema is used in getSchemaIndex, which allows us to check
     * whether a dataDef has already been created for that particular
     * schema and name pair. The look up should resolve references but
     * currently, it does not.
     */
    schema,
    required: [],
    targetGraphQLType, // May change due to allOf and oneOf resolution
    subDefinitions: undefined,
    links: saneLinks,
    graphQLTypeName: saneName,
    graphQLInputObjectTypeName: saneInputName,
  };

  // Used type names and defs of union and object types are pushed during creation
  if (
    targetGraphQLType === TargetGraphQLType.object ||
    targetGraphQLType === TargetGraphQLType.list ||
    targetGraphQLType === TargetGraphQLType.enum
  ) {
    data.usedTypeNames.push(saneName);
    data.usedTypeNames.push(saneInputName);

    // Add the def to the master list
    data.defs.push(def);
  }

  switch (targetGraphQLType) {
    case TargetGraphQLType.object:
      def.subDefinitions = {};

      if (
        typeof collapsedSchema.properties === 'object' &&
        Object.keys(collapsedSchema.properties).length > 0
      ) {
        addObjectPropertiesToDataDef(
          def,
          collapsedSchema,
          def.required,
          isInputObjectType,
          data,
          oas
        );
      } else {
        handleWarning({
          mitigationType: MitigationTypes.OBJECT_MISSING_PROPERTIES,
          message:
            `Schema ${JSON.stringify(schema)} does not have ` +
            `any properties`,
          data,
          log: preprocessingLog,
        });

        def.targetGraphQLType = TargetGraphQLType.json;
      }

      break;

    case TargetGraphQLType.list:
      if (typeof collapsedSchema.items === 'object') {
        // Break schema down into component parts
        // I.e. if it is an list type, create a reference to the list item type
        // Or if it is an object type, create references to all of the field types
        let itemsSchema = collapsedSchema.items;
        let itemsName = `${name}ListItem`;

        if ('$ref' in itemsSchema) {
          itemsName = itemsSchema.$ref.split('/').pop();
        }

        const extensionTypeName =
          collapsedSchema[Oas3Tools.OAS_GRAPHQL_EXTENSIONS.TypeName];

        const subDefinition = createDataDef(
          // Is this the correct classification for this name? It does not matter in the long run.
          {
            fromExtension: extensionTypeName,
            fromRef: itemsName,
          },
          itemsSchema as SchemaObject,
          isInputObjectType,
          data,
          oas
        );

        // Add list item reference
        def.subDefinitions = subDefinition;
      }

      break;

    case TargetGraphQLType.anyOfObject:
      if (Array.isArray(collapsedSchema.anyOf)) {
        /**
         * Sanity check
         *
         * Determining the targetGraphQLType should have checked the presence
         * of anyOf
         */
        createAnyOfObject(
          saneName,
          saneInputName,
          collapsedSchema,
          isInputObjectType,
          def,
          data,
          oas
        );
      } else {
        throw new Error(
          `OpenAPI-to-GraphQL error: Cannot create object ` +
            `from anyOf because there is no anyOf in ` +
            `schema '${JSON.stringify(schemaOrRef, null, 2)}'`
        );
      }
      break;

    case TargetGraphQLType.oneOfUnion:
      /**
       * Sanity check
       *
       * Determining the targetGraphQLType should have checked the presence
       * of oneOf
       */
      if (Array.isArray(collapsedSchema.oneOf)) {
        createOneOfUnion(
          saneName,
          saneInputName,
          collapsedSchema,
          isInputObjectType,
          def,
          data,
          oas
        );
      } else {
        throw new Error(
          `OpenAPI-to-GraphQL error: Cannot create union ` +
            `from oneOf because there is no oneOf in ` +
            `schema '${JSON.stringify(schemaOrRef, null, 2)}'`
        );
      }
      break;

    case TargetGraphQLType.json:
      def.targetGraphQLType = TargetGraphQLType.json;
      break;

    case null:
      // No target GraphQL type
      handleWarning({
        mitigationType: MitigationTypes.UNKNOWN_TARGET_TYPE,
        message: `No GraphQL target type could be identified for schema '${JSON.stringify(
          schema
        )}'.`,
        data,
        log: preprocessingLog,
      });

      def.targetGraphQLType = TargetGraphQLType.json;
      break;
  }

  return def;
}

/**
 * Returns the index of the data definition object in the given list that
 * contains the same schema and preferred name as the given one. Returns -1 if
 * that schema could not be found.
 */
function getSchemaIndex(
  preferredName: string,
  schema: SchemaObject,
  dataDefs: DataDefinition[]
): number {
  /**
   * TODO: instead of iterating through the whole list every time, create a
   * hashing function and store all of the DataDefinitions in a hashmap.
   */
  for (let index = 0; index < dataDefs.length; index++) {
    const def = dataDefs[index];
    /**
     * TODO: deepEquals is not sufficient. We also need to resolve references.
     * However, deepEquals should work for vast majority of cases.
     */

    if (preferredName === def.preferredName && deepEqual(schema, def.schema)) {
      return index;
    }
  }

  // The schema could not be found in the master list
  return -1;
}

/**
 * Determines the preferred name to use for schema regardless of name collisions.
 *
 * In other words, determines the ideal name for a schema.
 *
 * Similar to getSchemaName() except it does not check if the name has already
 * been taken.
 */
function getPreferredName(names: Oas3Tools.SchemaNames): string {
  if (typeof names.preferred === 'string') {
    return Oas3Tools.sanitize(names.preferred, Oas3Tools.CaseStyle.PascalCase); // CASE: preferred name already known
  } else if (typeof names.fromRef === 'string') {
    return Oas3Tools.sanitize(names.fromRef, Oas3Tools.CaseStyle.PascalCase); // CASE: name from reference
  } else if (typeof names.fromSchema === 'string') {
    return Oas3Tools.sanitize(names.fromSchema, Oas3Tools.CaseStyle.PascalCase); // CASE: name from schema (i.e., "title" property in schema)
  } else if (typeof names.fromPath === 'string') {
    return Oas3Tools.sanitize(names.fromPath, Oas3Tools.CaseStyle.PascalCase); // CASE: name from path
  } else {
    return 'PlaceholderName'; // CASE: placeholder name
  }
}

/**
 * Determines name to use for schema from previously determined schemaNames and
 * considering not reusing existing names.
 */
function getSchemaName(
  names: Oas3Tools.SchemaNames,
  usedNames: string[]
): string {
  if (Object.keys(names).length === 1 && typeof names.preferred === 'string') {
    throw new Error(
      `Cannot create data definition without name(s), excluding the ` +
        `preferred name.`
    );
  }

  let schemaName: string;

  if (typeof names.fromExtension === 'string') {
    const extensionTypeName = names.fromExtension;

    if (!Oas3Tools.isSanitized(extensionTypeName)) {
      throw new Error(
        `Cannot create type with name "${extensionTypeName}".\nYou ` +
          `provided "${extensionTypeName}" in ` +
          `${Oas3Tools.OAS_GRAPHQL_EXTENSIONS.TypeName}, but it is not ` +
          `GraphQL-safe."`
      );
    }

    if (usedNames.includes(extensionTypeName)) {
      throw new Error(
        `Cannot create type with name "${extensionTypeName}".\nYou provided ` +
          `"${names.fromExtension}" in ` +
          `${Oas3Tools.OAS_GRAPHQL_EXTENSIONS.TypeName}, but it conflicts ` +
          `with another type named "${extensionTypeName}".`
      );
    }

    if (!usedNames.includes(extensionTypeName)) {
      schemaName = names.fromExtension;
    }
  }

  // CASE: name from reference
  if (!schemaName && typeof names.fromRef === 'string') {
    const saneName = Oas3Tools.sanitize(
      names.fromRef,
      Oas3Tools.CaseStyle.PascalCase
    );
    if (!usedNames.includes(saneName)) {
      schemaName = names.fromRef;
    }
  }

  // CASE: name from schema (i.e., "title" property in schema)
  if (!schemaName && typeof names.fromSchema === 'string') {
    const saneName = Oas3Tools.sanitize(
      names.fromSchema,
      Oas3Tools.CaseStyle.PascalCase
    );
    if (!usedNames.includes(saneName)) {
      schemaName = names.fromSchema;
    }
  }

  // CASE: name from path
  if (!schemaName && typeof names.fromPath === 'string') {
    const saneName = Oas3Tools.sanitize(
      names.fromPath,
      Oas3Tools.CaseStyle.PascalCase
    );
    if (!usedNames.includes(saneName)) {
      schemaName = names.fromPath;
    }
  }

  // CASE: all names are already used - create approximate name
  if (!schemaName) {
    schemaName = Oas3Tools.sanitize(
      typeof names.fromExtension === 'string'
        ? names.fromExtension
        : typeof names.fromRef === 'string'
        ? names.fromRef
        : typeof names.fromSchema === 'string'
        ? names.fromSchema
        : typeof names.fromPath === 'string'
        ? names.fromPath
        : 'PlaceholderName',
      Oas3Tools.CaseStyle.PascalCase
    );
  }

  if (usedNames.includes(schemaName)) {
    let appendix = 2;

    /**
     * GraphQL Objects cannot share the name so if the name already exists in
     * the master list append an incremental number until the name does not
     * exist anymore.
     */
    while (usedNames.includes(`${schemaName}${appendix}`)) {
      appendix++;
    }
    schemaName = `${schemaName}${appendix}`;
  }

  return schemaName;
}

/**
 * Sanitize the keys of a link object
 */
function sanitizeLinks<TSource, TContext, TArgs>({
  links,
  data,
}: {
  links?: { [key: string]: LinkObject };
  data: PreprocessingData<TSource, TContext, TArgs>;
}): { [key: string]: LinkObject } {
  const saneLinks: { [key: string]: LinkObject } = {};
  if (typeof links === 'object') {
    Object.keys(links).forEach(linkKey => {
      const link = links[linkKey];

      const extensionFieldName =
        link[Oas3Tools.OAS_GRAPHQL_EXTENSIONS.FieldName];

      if (!Oas3Tools.isSanitized(extensionFieldName)) {
        throw new Error(
          `Cannot create link field with name ` +
            `"${extensionFieldName}".\nYou provided "${extensionFieldName}" in ` +
            `${Oas3Tools.OAS_GRAPHQL_EXTENSIONS.FieldName}, but it is not ` +
            `GraphQL-safe."`
        );
      }

      if (extensionFieldName in saneLinks) {
        throw new Error(
          `Cannot create link field with name ` +
            `"${extensionFieldName}".\nYou provided ` +
            `"${extensionFieldName}" in ` +
            `${Oas3Tools.OAS_GRAPHQL_EXTENSIONS.FieldName}, but it ` +
            `conflicts with another field named "${extensionFieldName}".`
        );
      }

      const linkFieldName = Oas3Tools.sanitize(
        extensionFieldName || linkKey,
        !data.options.simpleNames
          ? Oas3Tools.CaseStyle.camelCase
          : Oas3Tools.CaseStyle.simple
      );

      saneLinks[linkFieldName] = link;
    });
  }

  return saneLinks;
}

/**
 * Given an existing data definition, collapse the link object with the existing
 * one captured in the data definition.
 */
function collapseLinksIntoDataDefinition<TSource, TContext, TArgs>({
  additionalLinks,
  existingDataDef,
  data,
}: {
  additionalLinks?: { [key: string]: LinkObject };
  existingDataDef: DataDefinition;
  data: PreprocessingData<TSource, TContext, TArgs>;
}): void {
  /**
   * Collapse links if possible, i.e. if the current operation has links,
   * combine them with the prexisting ones
   */
  if (typeof existingDataDef.links === 'object') {
    // Check if there are any overlapping links
    Object.keys(existingDataDef.links).forEach(saneLinkKey => {
      if (
        !deepEqual(
          existingDataDef.links[saneLinkKey],
          additionalLinks[saneLinkKey]
        )
      ) {
        handleWarning({
          mitigationType: MitigationTypes.DUPLICATE_LINK_KEY,
          message:
            `Multiple operations with the same response body share the same sanitized ` +
            `link key '${saneLinkKey}' but have different link definitions ` +
            `'${JSON.stringify(existingDataDef.links[saneLinkKey])}' and ` +
            `'${JSON.stringify(additionalLinks[saneLinkKey])}'.`,
          data,
          log: preprocessingLog,
        });

        return;
      }
    });

    /**
     * Collapse the links
     *
     * Avoid overwriting preexisting links
     */
    existingDataDef.links = { ...additionalLinks, ...existingDataDef.links };
  } else {
    // No preexisting links, so simply assign the links
    existingDataDef.links = additionalLinks;
  }
}

/**
 * Recursively add all of the properties of an object to the data definition
 */
function addObjectPropertiesToDataDef<TSource, TContext, TArgs>(
  def: DataDefinition,
  schema: SchemaObject,
  required: string[],
  isInputObjectType: boolean,
  data: PreprocessingData<TSource, TContext, TArgs>,
  oas: Oas3
) {
  /**
   * Resolve all required properties
   *
   * TODO: required may contain duplicates, which is not necessarily a problem
   */
  if (Array.isArray(schema.required)) {
    schema.required.forEach(requiredProperty => {
      required.push(requiredProperty);
    });
  }

  for (let propertyKey in schema.properties) {
    if (!(propertyKey in def.subDefinitions)) {
      let propSchemaName = propertyKey;

      const propSchemaOrRef = schema.properties[propertyKey];

      let propSchema: SchemaObject;
      if (
        '$ref' in propSchemaOrRef &&
        typeof propSchemaOrRef.$ref === 'string'
      ) {
        propSchemaName = propSchemaOrRef.$ref.split('/').pop();
        propSchema = Oas3Tools.resolveRef(propSchemaOrRef.$ref, oas);
      } else {
        propSchema = propSchemaOrRef as SchemaObject;
      }

      const extensionTypeName =
        propSchema[Oas3Tools.OAS_GRAPHQL_EXTENSIONS.TypeName];

      const subDefinition = createDataDef(
        {
          fromExtension: extensionTypeName,
          fromRef: propSchemaName,
          fromSchema: propSchema.title, // TODO: Redundant because of fromRef but arguably, propertyKey is a better field name and title is a better type name
        },
        propSchema,
        isInputObjectType,
        data,
        oas
      );

      // Add field type references
      def.subDefinitions[propertyKey] = subDefinition;
    } else {
      handleWarning({
        mitigationType: MitigationTypes.DUPLICATE_FIELD_NAME,
        message:
          `By way of resolving 'allOf', multiple schemas contain ` +
          `properties with the same name, preventing consolidation. Cannot ` +
          `add property '${propertyKey}' from schema '${JSON.stringify(
            schema
          )}' ` +
          `to dataDefinition '${JSON.stringify(def)}'`,
        data,
        log: preprocessingLog,
      });
    }
  }
}

type MemberSchemaData = {
  allTargetGraphQLTypes: TargetGraphQLType[];
  allProperties: { [key: string]: SchemaObject | ReferenceObject }[];
  allRequired: string[];
};

/**
 * In the context of schemas that use keywords that combine member schemas,
 * collect data on certain aspects so it is all in one place for processing.
 */
function getMemberSchemaData<TSource, TContext, TArgs>(
  schemas: (SchemaObject | ReferenceObject)[],
  data: PreprocessingData<TSource, TContext, TArgs>,
  oas: Oas3
): MemberSchemaData {
  const result: MemberSchemaData = {
    allTargetGraphQLTypes: [], // Contains the target GraphQL types of all the member schemas
    allProperties: [], // Contains the properties of all the member schemas
    allRequired: [], // Contains the required of all the member schemas
  };

  schemas.forEach(schemaOrRef => {
    // Dereference schemas
    let schema: SchemaObject;
    if ('$ref' in schemaOrRef && typeof schemaOrRef.$ref === 'string') {
      schema = Oas3Tools.resolveRef(schemaOrRef.$ref, oas) as SchemaObject;
    } else {
      schema = schemaOrRef as SchemaObject;
    }

    // Consolidate target GraphQL type
    const memberTargetGraphQLType = Oas3Tools.getSchemaTargetGraphQLType(
      schema,
      data,
      oas
    );

    if (memberTargetGraphQLType) {
      result.allTargetGraphQLTypes.push(memberTargetGraphQLType);
    }

    // Consolidate properties
    if (schema.properties) {
      result.allProperties.push(schema.properties);
    }

    // Consolidate required
    if (schema.required) {
      result.allRequired = result.allRequired.concat(schema.required);
    }
  });

  return result;
}

function createAnyOfObject<TSource, TContext, TArgs>(
  saneName: string,
  saneInputName: string,
  collapsedSchema: SchemaObject,
  isInputObjectType: boolean,
  def: DataDefinition,
  data: PreprocessingData<TSource, TContext, TArgs>,
  oas: Oas3
) {
  /**
   * Used to find incompatible properties
   *
   * Store a properties from the base and member schemas. Start with the base
   * schema properties.
   *
   * If there are multiple properties with the same name, it only needs to store
   * the contents of one of them.
   *
   * If it is conflicting, add to incompatiable
   * properties; if not, do nothing.
   */
  const allProperties: {
    [propertyName: string]: SchemaObject | ReferenceObject;
  } = {};

  if ('properties' in collapsedSchema) {
    Object.entries(collapsedSchema.properties).forEach(
      ([propertyName, propertyObjectOrRef]) => {
        let property: SchemaObject;

        if (
          '$ref' in propertyObjectOrRef &&
          typeof propertyObjectOrRef.$ref === 'string'
        ) {
          property = Oas3Tools.resolveRef(propertyObjectOrRef.$ref, oas);
        } else {
          property = propertyObjectOrRef as SchemaObject;
        }

        allProperties[propertyName] = property;
      }
    );
  }

  // Store the names of properties with conflicting contents
  const incompatibleProperties = new Set<string>();

  // An array containing the properties of all member schemas
  const memberProperties: {
    [propertyName: string]: SchemaObject;
  }[] = [];

  collapsedSchema.anyOf.forEach(memberSchemaOrRef => {
    // Collapsed schema should already be recursively resolved
    let memberSchema: SchemaObject;
    if (
      '$ref' in memberSchemaOrRef &&
      typeof memberSchemaOrRef.$ref === 'string'
    ) {
      memberSchema = Oas3Tools.resolveRef(memberSchemaOrRef.$ref, oas);
    } else {
      memberSchema = memberSchemaOrRef as SchemaObject;
    }

    if (memberSchema.properties) {
      const properties: {
        [propertyName: string]: SchemaObject;
      } = {};

      Object.entries(memberSchema.properties).forEach(
        ([propertyName, propertyObjectOrRef]) => {
          let property: SchemaObject;

          if (
            '$ref' in propertyObjectOrRef &&
            typeof propertyObjectOrRef.$ref === 'string'
          ) {
            property = Oas3Tools.resolveRef(propertyObjectOrRef.$ref, oas);
          } else {
            property = propertyObjectOrRef as SchemaObject;
          }

          properties[propertyName] = property;
        }
      );

      memberProperties.push(properties);
    }
  });

  /**
   * TODO: Check for consistent properties across all member schemas and
   * make them into non-nullable properties by manipulating the
   * required field
   */

  /**
   * Add properties from the member schemas (from anyOf) as well as check
   * for incompatible properties (conflicting properties between member
   * schemas and other member schemas or the base schema)
   */
  memberProperties.forEach(properties => {
    Object.keys(properties).forEach(propertyName => {
      if (
        !incompatibleProperties.has(propertyName) && // Has not been already identified as a problematic property
        typeof allProperties[propertyName] === 'object' &&
        !deepEqual(properties[propertyName], allProperties[propertyName])
      ) {
        incompatibleProperties.add(propertyName);
      }

      /**
       * Save property to check in future iterations
       *
       * Can overwrite. If there is an incompatible property, we are
       * guaranteed to record it in incompatibleProperties
       */
      allProperties[propertyName] = properties[propertyName];
    });
  });

  def.subDefinitions = {};

  if (
    typeof collapsedSchema.properties === 'object' &&
    Object.keys(collapsedSchema.properties).length > 0
  ) {
    /**
     * TODO: Instead of creating the entire dataDefinition, disregard
     * incompatible properties.
     */
    addObjectPropertiesToDataDef(
      def,
      collapsedSchema,
      def.required,
      isInputObjectType,
      data,
      oas
    );
  }

  memberProperties.forEach(properties => {
    Object.keys(properties).forEach(propertyName => {
      if (!incompatibleProperties.has(propertyName)) {
        // Dereferenced by processing anyOfData
        const propertySchema = properties[propertyName] as SchemaObject;

        const extensionTypeName =
          propertySchema[Oas3Tools.OAS_GRAPHQL_EXTENSIONS.TypeName];

        const subDefinition = createDataDef(
          {
            fromExtension: extensionTypeName,
            fromRef: propertyName,
            fromSchema: propertySchema.title, // TODO: Currently not utilized because of fromRef but arguably, propertyKey is a better field name and title is a better type name
          },
          propertySchema,
          isInputObjectType,
          data,
          oas
        );

        /**
         * Add field type references
         * There should not be any collisions
         */
        def.subDefinitions[propertyName] = subDefinition;
      }
    });
  });

  // Add in incompatible properties
  incompatibleProperties.forEach(propertyName => {
    // TODO: add description
    def.subDefinitions[propertyName] = {
      targetGraphQLType: TargetGraphQLType.json,
    };
  });

  data.usedTypeNames.push(saneName);
  data.usedTypeNames.push(saneInputName);

  data.defs.push(def);

  def.targetGraphQLType = TargetGraphQLType.object;

  return def;
}

function createOneOfUnion<TSource, TContext, TArgs>(
  saneName: string,
  saneInputName: string,
  collapsedSchema: SchemaObject,
  isInputObjectType: boolean,
  def: DataDefinition,
  data: PreprocessingData<TSource, TContext, TArgs>,
  oas: Oas3
) {
  if (isInputObjectType) {
    handleWarning({
      mitigationType: MitigationTypes.INPUT_UNION,
      message: `Input object types cannot be composed of union types.`,
      data,
      log: preprocessingLog,
    });

    def.targetGraphQLType = TargetGraphQLType.json;
    return def;
  }

  def.subDefinitions = [];

  collapsedSchema.oneOf.forEach(memberSchemaOrRef => {
    // Collapsed schema should already be recursively resolved
    let fromRef: string;
    let memberSchema: SchemaObject;
    if (
      '$ref' in memberSchemaOrRef &&
      typeof memberSchemaOrRef.$ref === 'string'
    ) {
      fromRef = memberSchemaOrRef.$ref.split('/').pop();
      memberSchema = Oas3Tools.resolveRef(memberSchemaOrRef.$ref, oas);
    } else {
      memberSchema = memberSchemaOrRef as SchemaObject;
    }

    const extensionTypeName =
      memberSchema[Oas3Tools.OAS_GRAPHQL_EXTENSIONS.TypeName];

    const subDefinition = createDataDef(
      {
        fromExtension: extensionTypeName,
        fromRef,
        fromSchema: memberSchema.title,
        fromPath: `${saneName}Member`,
      },
      memberSchema,
      isInputObjectType,
      data,
      oas,
      def.links
    );
    (def.subDefinitions as DataDefinition[]).push(subDefinition);
  });

  // Not all member schemas may have been turned into GraphQL member types
  if (
    def.subDefinitions.length > 0 &&
    def.subDefinitions.every(subDefinition => {
      return subDefinition.targetGraphQLType === TargetGraphQLType.object;
    })
  ) {
    // Ensure all member schemas have been verified as object types
    data.usedTypeNames.push(saneName);
    data.usedTypeNames.push(saneInputName);

    data.defs.push(def);

    def.targetGraphQLType = TargetGraphQLType.oneOfUnion;
    return def;
  } else {
    handleWarning({
      mitigationType: MitigationTypes.COMBINE_SCHEMAS,
      message:
        `Schema '${JSON.stringify(def.schema)}' contains 'oneOf' so ` +
        `create a GraphQL union type but all member schemas are not` +
        `object types and union member types must be object types.`,
      mitigationAddendum: `Use arbitrary JSON type instead.`,
      data,
      log: preprocessingLog,
    });

    def.targetGraphQLType = TargetGraphQLType.json;
    return def;
  }
}
