// Copyright IBM Corp. 2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

/**
 * Utility functions around the OpenAPI Specification 3.
 */

// Type imports:
import { Oas2 } from './types/oas2';
import { TargetGraphQLType, Operation } from './types/operation';
import {
  Oas3,
  ServerObject,
  ParameterObject,
  SchemaObject,
  OperationObject,
  ResponsesObject,
  ResponseObject,
  PathItemObject,
  RequestBodyObject,
  ReferenceObject,
  LinksObject,
  LinkObject,
  MediaTypesObject,
  SecuritySchemeObject,
  SecurityRequirementObject,
} from './types/oas3';
import {
  PreprocessingData,
  ProcessedSecurityScheme,
} from './types/preprocessing_data';
import { InternalOptions } from './types/options';
import OpenAPIParser from '@readme/openapi-parser';

// Imports:
import * as Swagger2OpenAPI from 'swagger2openapi';
import * as OASValidator from 'oas-validator';
import debug from 'debug';
import { handleWarning, MitigationTypes } from './utils';
import * as jsonptr from 'json-ptr';
import * as pluralize from 'pluralize';

// Type definitions & exports:
export type SchemaNames = {
  // Sorted in the following priority order
  fromExtension?: string;
  fromRef?: string;
  fromSchema?: string;
  fromPath?: string;

  /**
   * Used when the preferred name is known, i.e. a new data def does not need to
   * be created
   */
  preferred?: string;
};

export type RequestSchemaAndNames = {
  payloadContentType?: string;
  payloadSchema?: SchemaObject;
  payloadSchemaNames?: SchemaNames;
  payloadRequired: boolean;
};

export type ResponseSchemaAndNames = {
  responseContentType?: string;
  responseSchema?: SchemaObject;
  responseSchemaNames?: SchemaNames;
  statusCode?: string;
};

const httpLog = debug('http');
const preprocessingLog = debug('preprocessing');

const translationLog = debug('translation');

// OAS constants
export enum HTTP_METHODS {
  'get' = 'get',
  'put' = 'put',
  'post' = 'post',
  'patch' = 'patch',
  'delete' = 'delete',
  'options' = 'options',
  'head' = 'head',
}

export const SUCCESS_STATUS_RX = /2[0-9]{2}|2XX/;

export enum OAS_GRAPHQL_EXTENSIONS {
  TypeName = 'x-graphql-type-name',
  FieldName = 'x-graphql-field-name',
  EnumMapping = 'x-graphql-enum-mapping',
}

const getType = (type: string | [string, null]): string | null => {
  if (typeof type === 'string') {
    return type;
  } else if (Array.isArray(type)) {
    return type[0];
  }
  return type;
};

/**
 * Given an HTTP method, convert it to the HTTP_METHODS enum
 */
export function methodToHttpMethod(method: string): HTTP_METHODS {
  switch (method.toLowerCase()) {
    case 'get':
      return HTTP_METHODS.get;

    case 'put':
      return HTTP_METHODS.put;

    case 'post':
      return HTTP_METHODS.post;

    case 'patch':
      return HTTP_METHODS.patch;

    case 'delete':
      return HTTP_METHODS.delete;

    case 'options':
      return HTTP_METHODS.options;

    case 'head':
      return HTTP_METHODS.head;

    default:
      throw new Error(`Invalid HTTP method '${method}'`);
  }
}

export function isOas2(spec: any): spec is Oas2 {
  return typeof spec.swagger === 'string' && /^2/.test(spec.swagger);
}

export function isOas3(spec: any): spec is Oas3 {
  return typeof spec.openapi === 'string' && /^3/.test(spec.openapi);
}

/**
 * Resolves on a validated OAS 3 for the given spec (OAS 2 or OAS 3), or rejects
 * if errors occur.
 */
export async function getValidOAS3(
  spec: Oas2 | Oas3,
  oasValidatorOptions: OpenAPIParser.Options,
  swagger2OpenAPIOptions: object,
  softValidate: boolean = false
): Promise<Oas3> {
  // CASE: translate
  if (isOas2(spec)) {
    preprocessingLog(
      `Received Swagger - going to translate to OpenAPI Specification...`
    );
    try {
      const { openapi } = await Swagger2OpenAPI.convertObj(
        spec,
        swagger2OpenAPIOptions
      );
      return openapi;
    } catch (error) {
      throw new Error(
        `Could not convert Swagger '${spec.info.title}' to OpenAPI Specification. ${error.message}`
      );
    }
    // CASE: validate
  } else if (isOas3(spec)) {
    preprocessingLog(`Received OpenAPI Specification - going to validate...`);
    if (!softValidate) {
      await OpenAPIParser.validate(
        JSON.parse(JSON.stringify(spec)),
        oasValidatorOptions
      );
    }
  } else {
    throw new Error(`Invalid specification provided`);
  }
  return spec;
}

/**
 * Counts the number of operations in an OAS.
 */
export function countOperations(oas: Oas3): number {
  let numOps = 0;
  for (let path in oas.paths) {
    for (let method in oas.paths[path]) {
      if (isHttpMethod(method)) {
        numOps++;
        if (oas.paths[path][method].callbacks) {
          for (let cbName in oas.paths[path][method].callbacks) {
            for (let cbPath in oas.paths[path][method].callbacks[cbName]) {
              numOps++;
            }
          }
        }
      }
    }
  }

  return numOps;
}

/**
 * Counts the number of operations that translate to queries in an OAS.
 */
export function countOperationsQuery(oas: Oas3): number {
  let numOps = 0;
  for (let path in oas.paths) {
    for (let method in oas.paths[path]) {
      if (isHttpMethod(method) && method.toLowerCase() === HTTP_METHODS.get) {
        numOps++;
      }
    }
  }
  return numOps;
}

/**
 * Counts the number of operations that translate to mutations in an OAS.
 */
export function countOperationsMutation(oas: Oas3): number {
  let numOps = 0;
  for (let path in oas.paths) {
    for (let method in oas.paths[path]) {
      if (isHttpMethod(method) && method.toLowerCase() !== HTTP_METHODS.get) {
        numOps++;
      }
    }
  }
  return numOps;
}

/**
 * Counts the number of operations that translate to subscriptions in an OAS.
 */
export function countOperationsSubscription(oas: Oas3): number {
  let numOps = 0;
  for (let path in oas.paths) {
    for (let method in oas.paths[path]) {
      if (
        isHttpMethod(method) &&
        method.toLowerCase() !== HTTP_METHODS.get &&
        oas.paths[path][method].callbacks
      ) {
        for (let cbName in oas.paths[path][method].callbacks) {
          for (let cbPath in oas.paths[path][method].callbacks[cbName]) {
            numOps++;
          }
        }
      }
    }
  }
  return numOps;
}

/**
 * Counts the number of operations with a payload definition in an OAS.
 */
export function countOperationsWithPayload(oas: Oas3): number {
  let numOps = 0;
  for (let path in oas.paths) {
    for (let method in oas.paths[path]) {
      if (
        isHttpMethod(method) &&
        typeof oas.paths[path][method].requestBody === 'object'
      ) {
        numOps++;
      }
    }
  }
  return numOps;
}

/**
 * Resolves the given reference in the given object.
 */
export function resolveRef<T = any>(ref: string, oas: Oas3): T {
  return jsonptr.JsonPointer.get(oas, ref) as T;
}

export const resolveAnyOf = (
  schema: SchemaObject | ReferenceObject
): SchemaObject | ReferenceObject => {
  const collapsedSchema: SchemaObject = JSON.parse(JSON.stringify(schema));

  if ('anyOf' in collapsedSchema) {
    const types = collapsedSchema.anyOf.map(type => {
      if ('$ref' in type) {
        return 'object';
      }
      return type.type;
    });

    if (types.length === 1) {
      collapsedSchema.type = types[0];
    } else {
      collapsedSchema.type = 'object';
    }
  }

  return collapsedSchema;
};

/**
 * Recursively traverse a schema and resolve allOf by appending the data to the
 * parent schema
 */
export function resolveAllOf<TSource, TContext, TArgs>(
  schema: SchemaObject | ReferenceObject,
  references: { [reference: string]: SchemaObject },
  data: PreprocessingData<TSource, TContext, TArgs>,
  oas: Oas3
): SchemaObject {
  // Dereference schema
  if ('$ref' in schema && typeof schema.$ref === 'string') {
    if (schema.$ref in references) {
      return references[schema.$ref];
    }

    const reference = schema.$ref;

    schema = resolveRef(schema.$ref, oas) as SchemaObject;
    references[reference] = schema;
  }

  /**
   * TODO: Is there a better method to copy the schema?
   *
   * Copy the schema
   */
  const collapsedSchema: SchemaObject = JSON.parse(JSON.stringify(schema));

  // Resolve allOf
  if (Array.isArray(collapsedSchema.allOf)) {
    collapsedSchema.allOf.forEach(memberSchema => {
      const collapsedMemberSchema = resolveAllOf(
        memberSchema,
        references,
        data,
        oas
      );

      // Collapse type if applicable
      if (collapsedMemberSchema.type) {
        if (!collapsedSchema.type) {
          collapsedSchema.type = collapsedMemberSchema.type;

          // Check for incompatible schema type
        } else if (collapsedSchema.type !== collapsedMemberSchema.type) {
          handleWarning({
            mitigationType: MitigationTypes.UNRESOLVABLE_SCHEMA,
            message:
              `Resolving 'allOf' field in schema '${JSON.stringify(
                collapsedSchema
              )}' ` + `results in incompatible schema type.`,
            data,
            log: preprocessingLog,
          });
        }
      }

      // Collapse properties if applicable
      if ('properties' in collapsedMemberSchema) {
        if (!('properties' in collapsedSchema)) {
          collapsedSchema.properties = {};
        }

        Object.entries(collapsedMemberSchema.properties).forEach(
          ([propertyName, property]) => {
            if (!(propertyName in collapsedSchema.properties)) {
              collapsedSchema.properties[propertyName] = property;

              // Conflicting property
            } else {
              handleWarning({
                mitigationType: MitigationTypes.UNRESOLVABLE_SCHEMA,
                message:
                  `Resolving 'allOf' field in schema '${JSON.stringify(
                    collapsedSchema
                  )}' ` +
                  `results in incompatible property field '${propertyName}'.`,
                data,
                log: preprocessingLog,
              });
            }
          }
        );
      }

      // Collapse oneOf if applicable
      if ('oneOf' in collapsedMemberSchema) {
        if (!('oneOf' in collapsedSchema)) {
          collapsedSchema.oneOf = [];
        }

        collapsedMemberSchema.oneOf.forEach(oneOfProperty => {
          collapsedSchema.oneOf.push(oneOfProperty);
        });
      }

      // Collapse anyOf if applicable
      if ('anyOf' in collapsedMemberSchema) {
        if (!('anyOf' in collapsedSchema)) {
          collapsedSchema.anyOf = [];
        }

        collapsedMemberSchema.anyOf.forEach(anyOfProperty => {
          collapsedSchema.anyOf.push(anyOfProperty);
        });
      }

      // Collapse required if applicable
      if ('required' in collapsedMemberSchema) {
        if (!('required' in collapsedSchema)) {
          collapsedSchema.required = [];
        }

        collapsedMemberSchema.required.forEach(requiredProperty => {
          if (!collapsedSchema.required.includes(requiredProperty)) {
            collapsedSchema.required.push(requiredProperty);
          }
        });
      }
    });
  }

  return collapsedSchema;
}

/**
 * Returns the base URL to use for the given operation.
 */
export function getBaseUrl(operation: Operation): string {
  // Check for servers:
  if (!Array.isArray(operation.servers) || operation.servers.length === 0) {
    throw new Error(
      `No servers defined for operation '${operation.operationString}'`
    );
  }

  // Check for local servers
  if (Array.isArray(operation.servers) && operation.servers.length > 0) {
    const url = buildUrl(operation.servers[0]);

    if (Array.isArray(operation.servers) && operation.servers.length > 1) {
      httpLog(`Warning: Randomly selected first server '${url}'`);
    }

    return url.replace(/\/$/, '');
  }

  const oas = operation.oas;

  if (Array.isArray(oas.servers) && oas.servers.length > 0) {
    const url = buildUrl(oas.servers[0]);

    if (Array.isArray(oas.servers) && oas.servers.length > 1) {
      httpLog(`Warning: Randomly selected first server '${url}'`);
    }

    return url.replace(/\/$/, '');
  }

  throw new Error('Cannot find a server to call');
}

/**
 * Returns the default URL for a given OAS server object.
 */
function buildUrl(server: ServerObject): string {
  let url = server.url;

  // Replace with variable defaults, if applicable
  if (
    typeof server.variables === 'object' &&
    Object.keys(server.variables).length > 0
  ) {
    for (let variableKey in server.variables) {
      // TODO: check for default? Would be invalid OAS
      url = url.replace(
        `{${variableKey}}`,
        server.variables[variableKey].default.toString()
      );
    }
  }

  return url;
}

/**
 * Returns object/array/scalar where all object keys (if applicable) are
 * sanitized.
 */
export function sanitizeObjectKeys(
  obj: any, // obj does not necessarily need to be an object
  caseStyle: CaseStyle = CaseStyle.camelCase
): any {
  const cleanKeys = (obj: any): any => {
    // Case: no (response) data
    if (obj === null || typeof obj === 'undefined') {
      return null;

      // Case: array
    } else if (Array.isArray(obj)) {
      return obj.map(cleanKeys);

      // Case: object
    } else if (typeof obj === 'object') {
      const res: object = {};

      for (const key in obj) {
        const saneKey = sanitize(key, caseStyle);

        if (Object.prototype.hasOwnProperty.call(obj, key)) {
          res[saneKey] = cleanKeys(obj[key]);
        }
      }

      return res;

      // Case: scalar
    } else {
      return obj;
    }
  };

  return cleanKeys(obj);
}

/**
 * Desanitizes keys in given object by replacing them with the keys stored in
 * the given mapping.
 */
export function desanitizeObjectKeys(
  obj: object | Array<any>,
  mapping: object = {}
): object | Array<any> {
  const replaceKeys = obj => {
    if (obj === null) {
      return null;
    } else if (Array.isArray(obj)) {
      return obj.map(replaceKeys);
    } else if (typeof obj === 'object') {
      const res = {};
      for (let key in obj) {
        if (key in mapping) {
          const rawKey = mapping[key];
          if (Object.prototype.hasOwnProperty.call(obj, key)) {
            res[rawKey] = replaceKeys(obj[key]);
          }
        } else {
          res[key] = replaceKeys(obj[key]);
        }
      }
      return res;
    } else {
      return obj;
    }
  };
  return replaceKeys(obj);
}

/**
 * Returns the GraphQL type that the provided schema should be made into
 */
export function getSchemaTargetGraphQLType<TSource, TContext, TArgs>(
  schemaOrRef: SchemaObject | ReferenceObject,
  data: PreprocessingData<TSource, TContext, TArgs>,
  oas: Oas3
): TargetGraphQLType | null {
  let schema: SchemaObject;
  if ('$ref' in schemaOrRef && typeof schemaOrRef.$ref === 'string') {
    schema = resolveRef(schemaOrRef.$ref, oas);
  } else {
    schema = schemaOrRef as SchemaObject;
  }

  // TODO: Need to resolve allOf here as well.

  // CASE: Check for nested or concurrent anyOf and oneOf
  if (
    // TODO: Should also consider if the member schema contains type data
    (Array.isArray(schema.anyOf) && Array.isArray(schema.oneOf)) || // anyOf and oneOf used concurrently
    hasNestedAnyOfUsage(schema, oas) ||
    hasNestedOneOfUsage(schema, oas)
  ) {
    handleWarning({
      mitigationType: MitigationTypes.COMBINE_SCHEMAS,
      message:
        `Schema '${JSON.stringify(schema)}' contains either both ` +
        `'anyOf' and 'oneOf' or nested 'anyOf' and 'oneOf' which ` +
        `is currently not supported.`,
      mitigationAddendum: `Use arbitrary JSON type instead.`,
      data,
      log: preprocessingLog,
    });

    return TargetGraphQLType.json;
  }

  if (Array.isArray(schema.anyOf)) {
    return GetAnyOfTargetGraphQLType(schema, data, oas);
  }

  if (Array.isArray(schema.oneOf)) {
    return GetOneOfTargetGraphQLType(schema, data, oas);
  }

  // CASE: enum
  if (Array.isArray(schema.enum)) {
    return TargetGraphQLType.enum;
  }

  const type = getType(schema.type);

  // CASE: object
  if (type === 'object' || typeof schema.properties === 'object') {
    // TODO: additionalProperties is more like a flag than a type itself
    // CASE: arbitrary JSON
    if (typeof schema.additionalProperties === 'object') {
      return TargetGraphQLType.json;
    } else {
      return TargetGraphQLType.object;
    }
  }

  // CASE: array
  if (type === 'array' || 'items' in schema) {
    return TargetGraphQLType.list;
  }

  // Special edge cases involving the schema format
  if (typeof schema.format === 'string') {
    if (type === 'integer' && schema.format === 'int64') {
      return TargetGraphQLType.bigint;
      // CASE: file upload
    } else if (type === 'string' && schema.format === 'binary') {
      return TargetGraphQLType.upload;
      // CASE: id
    } else if (
      type === 'string' &&
      (schema.format === 'uuid' ||
        // Custom ID format
        (Array.isArray(data.options.idFormats) &&
          data.options.idFormats.includes(schema.format)))
    ) {
      return TargetGraphQLType.id;
    }
  }

  switch (type) {
    case 'string':
      return TargetGraphQLType.string;

    case 'number':
      return TargetGraphQLType.float;

    case 'integer':
      return TargetGraphQLType.integer;

    case 'boolean':
      return TargetGraphQLType.boolean;

    default:
    // Error: unsupported schema type
  }

  return null;
}

/**
 * Check to see if there are cases of nested oneOf fields in the member schemas
 *
 * We currently cannot handle complex cases of oneOf and anyOf
 */
function hasNestedOneOfUsage(schema: SchemaObject, oas: Oas3): boolean {
  // TODO: Should also consider if the member schema contains type data
  return (
    Array.isArray(schema.oneOf) &&
    schema.oneOf.some(memberSchemaOrRef => {
      let memberSchema: SchemaObject;
      if (
        '$ref' in memberSchemaOrRef &&
        typeof memberSchemaOrRef.$ref === 'string'
      ) {
        memberSchema = resolveRef(memberSchemaOrRef.$ref, oas);
      } else {
        memberSchema = memberSchemaOrRef as SchemaObject;
      }

      return (
        /**
         * anyOf and oneOf are nested
         *
         * Nested oneOf would result in nested unions which are not allowed by
         * GraphQL
         */
        Array.isArray(memberSchema.anyOf) || Array.isArray(memberSchema.oneOf)
      );
    })
  );
}

/**
 * Check to see if there are cases of nested anyOf fields in the member schemas
 *
 * We currently cannot handle complex cases of oneOf and anyOf
 */
function hasNestedAnyOfUsage(schema: SchemaObject, oas: Oas3): boolean {
  // TODO: Should also consider if the member schema contains type data
  return (
    Array.isArray(schema.anyOf) &&
    schema.anyOf.some(memberSchemaOrRef => {
      let memberSchema: SchemaObject;

      if (
        '$ref' in memberSchemaOrRef &&
        typeof memberSchemaOrRef.$ref === 'string'
      ) {
        memberSchema = resolveRef(memberSchemaOrRef.$ref, oas);
      } else {
        memberSchema = memberSchemaOrRef as SchemaObject;
      }

      return (
        // anyOf and oneOf are nested
        Array.isArray(memberSchema.anyOf) || Array.isArray(memberSchema.oneOf)
      );
    })
  );
}

function GetAnyOfTargetGraphQLType<TSource, TContext, TArgs>(
  schema: SchemaObject,
  data: PreprocessingData<TSource, TContext, TArgs>,
  oas: Oas3
): TargetGraphQLType {
  // Identify the type of the base schema, meaning ignoring the anyOf
  const schemaWithNoAnyOf = { ...schema };
  delete schemaWithNoAnyOf.anyOf;
  const baseTargetType = getSchemaTargetGraphQLType(
    schemaWithNoAnyOf,
    data,
    oas
  );

  // Target GraphQL types of all the member schemas
  const memberTargetTypes: TargetGraphQLType[] = [];
  schema.anyOf.forEach(memberSchema => {
    const memberTargetType = getSchemaTargetGraphQLType(
      memberSchema,
      data,
      oas
    );

    if (memberTargetType !== null) {
      memberTargetTypes.push(memberTargetType);
    }
  });

  if (memberTargetTypes.length > 0) {
    const firstMemberTargetType = memberTargetTypes[0];
    const consistentMemberTargetTypes = memberTargetTypes.every(targetType => {
      return targetType === firstMemberTargetType;
    });

    if (consistentMemberTargetTypes) {
      if (baseTargetType !== null) {
        if (baseTargetType === firstMemberTargetType) {
          if (baseTargetType === 'object') {
            // Base schema and member schema types are object types
            return TargetGraphQLType.anyOfObject;
          } else {
            // Base schema and member schema types but no object types
            return baseTargetType;
          }
        } else {
          // Base schema and member schema types are not consistent
          return TargetGraphQLType.json;
        }
      } else {
        if (firstMemberTargetType === TargetGraphQLType.object) {
          return TargetGraphQLType.anyOfObject;
        } else {
          return firstMemberTargetType;
        }
      }
    } else {
      // Member schema types are not consistent
      return TargetGraphQLType.json;
    }
  } else {
    // No member schema types, therefore use the base schema type
    return baseTargetType;
  }
}

function GetOneOfTargetGraphQLType<TSource, TContext, TArgs>(
  schema: SchemaObject,
  data: PreprocessingData<TSource, TContext, TArgs>,
  oas: Oas3
): TargetGraphQLType {
  // Identify the type of the base schema, meaning ignoring the oneOf
  const schemaWithNoOneOf = { ...schema };
  delete schemaWithNoOneOf.oneOf;
  const baseTargetType = getSchemaTargetGraphQLType(
    schemaWithNoOneOf,
    data,
    oas
  );

  // Target GraphQL types of all the member schemas
  const memberTargetTypes: TargetGraphQLType[] = [];
  schema.oneOf.forEach(memberSchema => {
    const collapsedMemberSchema = resolveAllOf(memberSchema, {}, data, oas);
    const memberTargetType = getSchemaTargetGraphQLType(
      collapsedMemberSchema,
      data,
      oas
    );

    if (memberTargetType !== null) {
      memberTargetTypes.push(memberTargetType);
    }
  });

  if (memberTargetTypes.length > 0) {
    const firstMemberTargetType = memberTargetTypes[0];
    const consistentMemberTargetTypes = memberTargetTypes.every(targetType => {
      return targetType === firstMemberTargetType;
    });

    if (consistentMemberTargetTypes) {
      if (baseTargetType !== null) {
        if (baseTargetType === firstMemberTargetType) {
          if (baseTargetType === 'object') {
            // Base schema and member schema types are object types
            return TargetGraphQLType.oneOfUnion;
          } else {
            // Base schema and member schema types but no object types
            return baseTargetType;
          }
        } else {
          // Base schema and member schema types are not consistent
          return TargetGraphQLType.json;
        }
      } else {
        if (firstMemberTargetType === TargetGraphQLType.object) {
          return TargetGraphQLType.oneOfUnion;
        } else {
          return firstMemberTargetType;
        }
      }
    } else {
      // Member schema types are not consistent
      return TargetGraphQLType.json;
    }
  } else {
    // No member schema types, therefore use the base schema type
    return baseTargetType;
  }
}

/**
 * Identifies common path components in the given list of paths. Returns these
 * components as well as an updated list of paths where the common prefix was
 * removed.
 */
function extractBasePath(paths: string[]): {
  basePath: string;
  updatedPaths: string[];
} {
  if (paths.length <= 1) {
    return {
      basePath: '/',
      updatedPaths: paths,
    };
  }

  let basePathComponents: string[] = paths[0].split('/');

  for (let path of paths) {
    if (basePathComponents.length === 0) {
      break;
    }
    const pathComponents = path.split('/');
    for (let i = 0; i < pathComponents.length; i++) {
      if (i < basePathComponents.length) {
        if (pathComponents[i] !== basePathComponents[i]) {
          basePathComponents = basePathComponents.slice(0, i);
        }
      } else {
        break;
      }
    }
  }

  const updatedPaths = paths.map(path =>
    path.split('/').slice(basePathComponents.length).join('/')
  );

  let basePath =
    basePathComponents.length === 0 ||
    (basePathComponents.length === 1 && basePathComponents[0] === '')
      ? '/'
      : basePathComponents.join('/');

  return {
    basePath,
    updatedPaths,
  };
}

function isIdParam(part) {
  return /^{.*(id|name|key).*}$/gi.test(part);
}

function isSingularParam(part, nextPart) {
  return `\{${pluralize.singular(part)}\}` === nextPart;
}

/**
 * Infers a resource name from the given URL path.
 *
 * For example, turns "/users/{userId}/car" into "userCar".
 */
export function inferResourceNameFromPath(path: string): string {
  const parts = path.split('/');
  let pathNoPathParams = parts.reduce((path, part, i) => {
    if (!/{/g.test(part)) {
      if (
        parts[i + 1] &&
        (isIdParam(parts[i + 1]) || isSingularParam(part, parts[i + 1]))
      ) {
        return path + capitalize(pluralize.singular(part));
      } else {
        return path + capitalize(part);
      }
    } else {
      return path;
    }
  }, '');

  return pathNoPathParams;
}

/**
 * Returns the request schema (if any) for the given operation,
 * a dictionary of names from different sources (if available), and whether the
 * request schema is required for the operation.
 */
export function getRequestSchemaAndNames(
  path: string,
  method: HTTP_METHODS,
  operation: OperationObject,
  oas: Oas3
): RequestSchemaAndNames {
  let payloadContentType: string; // randomly selected content-type, prioritizing application/json
  let requestBodyObject: RequestBodyObject; // request object
  let payloadSchema: SchemaObject; // request schema with given content-type
  let payloadSchemaNames: SchemaNames; // dictionary of names
  let payloadRequired = false;

  // Get request body
  const requestBodyObjectOrRef = operation?.requestBody;
  if (
    typeof requestBodyObjectOrRef === 'object' &&
    requestBodyObjectOrRef !== null
  ) {
    // Resolve reference if applicable. Make sure we have a RequestBodyObject:
    if (
      '$ref' in requestBodyObjectOrRef &&
      typeof requestBodyObjectOrRef.$ref === 'string'
    ) {
      requestBodyObject = resolveRef(requestBodyObjectOrRef.$ref, oas);
    } else {
      requestBodyObject = requestBodyObjectOrRef as RequestBodyObject;
    }

    if (typeof requestBodyObject === 'object' && requestBodyObject !== null) {
      // Determine if request body is required:
      payloadRequired =
        typeof requestBodyObject?.required === 'boolean'
          ? requestBodyObject?.required
          : false;

      // Determine content-type
      const content: MediaTypesObject = requestBodyObject?.content;
      if (
        typeof content === 'object' &&
        content !== null &&
        Object.keys(content).length > 0
      ) {
        // Prioritize content-type JSON
        if ('application/json' in content) {
          payloadContentType = 'application/json';
        } else if ('application/x-www-form-urlencoded' in content) {
          payloadContentType = 'application/x-www-form-urlencoded';
        } else {
          // Pick first (random) content type
          const randomContentType = Object.keys(content)[0];
          payloadContentType = randomContentType;
        }

        if (
          payloadContentType === 'application/json' ||
          payloadContentType === '*/*' ||
          payloadContentType === 'application/x-www-form-urlencoded' ||
          payloadContentType === 'multipart/form-data'
        ) {
          // Name extracted from a reference, if applicable
          let fromRef: string;

          // Determine payload schema
          const payloadSchemaOrRef = content?.[payloadContentType]?.schema;
          if (
            typeof payloadSchemaOrRef === 'object' &&
            payloadSchemaOrRef !== null
          ) {
            // Resolve payload schema reference if applicable
            if (
              '$ref' in payloadSchemaOrRef &&
              typeof payloadSchemaOrRef.$ref === 'string'
            ) {
              fromRef = payloadSchemaOrRef.$ref.split('/').pop();
              payloadSchema = resolveRef(payloadSchemaOrRef.$ref, oas);
            } else {
              payloadSchema = payloadSchemaOrRef as SchemaObject;
            }
          }

          // Determine possible schema names
          payloadSchemaNames = {
            fromExtension: payloadSchema[OAS_GRAPHQL_EXTENSIONS.TypeName],
            fromRef,
            fromSchema: payloadSchema?.title,
            fromPath: inferResourceNameFromPath(path),
          };

          /**
           * Edge case: if request body content-type is not application/json or
           * application/x-www-form-urlencoded, do not parse it.
           *
           * Instead, treat the request body as a black box and send it as a string
           * with the proper content-type header
           */
        } else {
          const saneContentTypeName = uncapitalize(
            payloadContentType.split('/').reduce((name, term) => {
              return name + capitalize(term);
            })
          );

          let description = `String represents payload of content type '${payloadContentType}'`;

          if (typeof payloadSchema?.description === 'string') {
            description += `\n\nOriginal top level description: '${payloadSchema.description}'`;
          }

          // Replacement schema to avoid parsing
          payloadSchema = {
            description,
            type: 'string',
          };

          // Determine possible schema names
          payloadSchemaNames = {
            fromPath: saneContentTypeName,
          };
        }
      }
    }
  }

  return {
    payloadContentType,
    payloadSchema,
    payloadSchemaNames,
    payloadRequired,
  };
}

/**
 * Returns the response schema for the given operation,
 * a successful status code, and a dictionary of names from different sources
 * (if available).
 */
export function getResponseSchemaAndNames<TSource, TContext, TArgs>(
  path: string,
  method: HTTP_METHODS,
  operation: OperationObject,
  oas: Oas3,
  data: PreprocessingData<TSource, TContext, TArgs>,
  options: InternalOptions<TSource, TContext, TArgs>
): ResponseSchemaAndNames {
  let responseContentType: string; // randomly selected content-type, prioritizing application/json
  let responseObject: ResponseObject; // response object
  let responseSchema: SchemaObject; // response schema with given content-type
  let responseSchemaNames: SchemaNames; // dictionary of names

  const statusCode = getResponseStatusCode(path, method, operation, oas, data);

  // Get response object
  const responseObjectOrRef = operation?.responses?.[statusCode];
  if (typeof responseObjectOrRef === 'object' && responseObjectOrRef !== null) {
    if (
      '$ref' in responseObjectOrRef &&
      typeof responseObjectOrRef.$ref === 'string'
    ) {
      responseObject = resolveRef(responseObjectOrRef.$ref, oas);
    } else {
      responseObject = responseObjectOrRef as ResponseObject;
    }

    // Determine content-type
    if (typeof responseObject === 'object' && responseObject !== null) {
      const content: MediaTypesObject = responseObject?.content;
      if (
        typeof content === 'object' &&
        content !== null &&
        Object.keys(content).length > 0
      ) {
        // Prioritize content-type JSON
        if ('application/json' in content) {
          responseContentType = 'application/json';
        } else {
          // Pick first (random) content type
          const randomContentType = Object.keys(content)[0];
          responseContentType = randomContentType;
        }

        if (
          responseContentType === 'application/json' ||
          responseContentType === '*/*'
        ) {
          // Name from reference, if applicable
          let fromRef: string;

          // Determine response schema
          const responseSchemaOrRef =
            responseObject?.content?.[responseContentType]?.schema;
          // Resolve response schema reference if applicable
          if (
            '$ref' in responseSchemaOrRef &&
            typeof responseSchemaOrRef.$ref === 'string'
          ) {
            fromRef = responseSchemaOrRef.$ref.split('/').pop();
            responseSchema = resolveRef(responseSchemaOrRef.$ref, oas);
          } else {
            responseSchema = responseSchemaOrRef as SchemaObject;
          }

          // Determine possible schema names
          responseSchemaNames = {
            fromExtension: responseSchema[OAS_GRAPHQL_EXTENSIONS.TypeName],
            fromRef,
            fromSchema: responseSchema?.title,
            fromPath: inferResourceNameFromPath(path),
          };

          /**
           * Edge case: if response body content-type is not application/json,
           * do not parse.
           */
        } else {
          let description =
            'Placeholder to access non-application/json response bodies';

          if (typeof responseSchema?.description === 'string') {
            description += `\n\nOriginal top level description: '${responseSchema.description}'`;
          }

          // Replacement schema to avoid parsing
          responseSchema = {
            description,
            type: 'string',
          };

          // Determine possible schema names
          responseSchemaNames = {
            fromExtension: responseSchema?.[OAS_GRAPHQL_EXTENSIONS.TypeName],
            fromSchema: responseSchema?.title,
            fromPath: inferResourceNameFromPath(path),
          };
        }

        return {
          responseContentType,
          responseSchema,
          responseSchemaNames,
          statusCode,
        };
      }
    }
  }

  // No response schema
  if (options.fillEmptyResponses) {
    return {
      responseSchemaNames: {
        fromPath: inferResourceNameFromPath(path),
      },
      responseSchema: {
        description:
          'Placeholder to support operations with no response schema',
        type: 'string',
      },
    };
  } else {
    return {};
  }
}

/**
 * Returns a success status code for the given operation
 */
export function getResponseStatusCode<TSource, TContext, TArgs>(
  path: string,
  method: string,
  operation: OperationObject,
  oas: Oas3,
  data: PreprocessingData<TSource, TContext, TArgs>
): string {
  if (typeof operation.responses === 'object' && operation.responses !== null) {
    const codes = Object.keys(operation.responses);
    const successCodes = codes.filter(code => {
      return SUCCESS_STATUS_RX.test(code);
    });

    if (successCodes.length === 1) {
      return successCodes[0];
    } else if (successCodes.length > 1) {
      // Select a random success code
      handleWarning({
        mitigationType: MitigationTypes.MULTIPLE_RESPONSES,
        message:
          `Operation '${formatOperationString(
            method,
            path,
            oas.info.title
          )}' ` +
          `contains multiple possible successful response object ` +
          `(HTTP code 200-299 or 2XX). Only one can be chosen.`,
        mitigationAddendum:
          `The response object with the HTTP code ` +
          `${successCodes[0]} will be selected`,
        data,
        log: translationLog,
      });

      return successCodes[0];
    }
  }
}

/**
 * Returns a hash containing the links in the given operation.
 */
export function getLinks<TSource, TContext, TArgs>(
  path: string,
  method: HTTP_METHODS,
  operation: OperationObject,
  oas: Oas3,
  data: PreprocessingData<TSource, TContext, TArgs>
): { [key: string]: LinkObject } {
  const links = {};
  const statusCode = getResponseStatusCode(path, method, operation, oas, data);
  if (!statusCode) {
    return links;
  }

  if (typeof operation.responses === 'object') {
    const responses: ResponsesObject = operation.responses;
    if (typeof responses[statusCode] === 'object') {
      const responseObjectOrRef = responses[statusCode];

      let response: ResponseObject;
      if (
        '$ref' in responseObjectOrRef &&
        typeof responseObjectOrRef.$ref === 'string'
      ) {
        response = resolveRef(responseObjectOrRef.$ref, oas);
      } else {
        response = responseObjectOrRef as ResponseObject;
      }

      if (typeof response.links === 'object') {
        const epLinks: LinksObject = response.links;
        for (let linkKey in epLinks) {
          const linkObjectOrRef = epLinks[linkKey];

          let link: LinkObject;
          if (
            '$ref' in linkObjectOrRef &&
            typeof linkObjectOrRef.$ref === 'string'
          ) {
            link = resolveRef(linkObjectOrRef.$ref, oas);
          } else {
            link = linkObjectOrRef as LinkObject;
          }

          links[linkKey] = link;
        }
      }
    }
  }
  return links;
}

/**
 * Returns the list of parameters in the given operation.
 */
export function getParameters(
  path: string,
  method: HTTP_METHODS,
  operation: OperationObject,
  pathItem: PathItemObject,
  oas: Oas3
): ParameterObject[] {
  let parameters = [];

  if (!isHttpMethod(method)) {
    translationLog(
      `Warning: attempted to get parameters for ${method} ${path}, ` +
        `which is not an operation.`
    );
    return parameters;
  }

  // First, consider parameters in Path Item Object:
  const pathParams = pathItem.parameters;
  if (Array.isArray(pathParams)) {
    const pathItemParameters: ParameterObject[] = pathParams.map(p => {
      if ('$ref' in p && typeof p.$ref === 'string') {
        // Here we know we have a parameter object:
        return resolveRef(p.$ref, oas) as ParameterObject;
      } else {
        // Here we know we have a parameter object:
        return p as ParameterObject;
      }
    });
    parameters = parameters.concat(pathItemParameters);
  }

  // Second, consider parameters in Operation Object:
  const opObjectParameters = operation.parameters;
  if (Array.isArray(opObjectParameters)) {
    const operationParameters: ParameterObject[] = opObjectParameters.map(p => {
      if ('$ref' in p && typeof p.$ref === 'string') {
        // Here we know we have a parameter object:
        return resolveRef(p.$ref, oas);
      } else {
        // Here we know we have a parameter object:
        return p as ParameterObject;
      }
    });
    parameters = parameters.concat(operationParameters);
  }

  return parameters;
}

/**
 * Returns an array of server objects for the operation at the given path and
 * method. Considers in the following order: global server definitions,
 * definitions at the path item, definitions at the operation, or the OAS
 * default.
 */
export function getServers(
  operation: OperationObject,
  pathItem: PathItemObject,
  oas: Oas3
): ServerObject[] {
  let servers = [];
  // Global server definitions:
  if (Array.isArray(oas.servers) && oas.servers.length > 0) {
    servers = oas.servers;
  }

  // First, consider servers defined on the path
  if (Array.isArray(pathItem.servers) && pathItem.servers.length > 0) {
    servers = pathItem.servers;
  }

  // Second, consider servers defined on the operation
  if (Array.isArray(operation.servers) && operation.servers.length > 0) {
    servers = operation.servers;
  }

  // Default, in case there is no server:
  if (servers.length === 0) {
    let server: ServerObject = {
      url: '/', // TODO: avoid double-slashes
    };
    servers.push(server);
  }

  return servers;
}

/**
 * Returns a map of security scheme definitions, identified by keys. Resolves
 * possible references.
 */
export function getSecuritySchemes(oas: Oas3): {
  [schemeKey: string]: SecuritySchemeObject;
} {
  // Collect all security schemes:
  const securitySchemes: { [schemeKey: string]: SecuritySchemeObject } = {};
  if (
    typeof oas.components === 'object' &&
    typeof oas.components.securitySchemes === 'object'
  ) {
    for (let schemeKey in oas.components.securitySchemes) {
      const securitySchemeOrRef = oas.components.securitySchemes[schemeKey];

      // Ensure we have actual SecuritySchemeObject:
      if (
        '$ref' in securitySchemeOrRef &&
        typeof securitySchemeOrRef.$ref === 'string'
      ) {
        // Result of resolution will be SecuritySchemeObject:
        securitySchemes[schemeKey] = resolveRef(securitySchemeOrRef.$ref, oas);
      } else {
        // We already have a SecuritySchemeObject:
        securitySchemes[schemeKey] =
          securitySchemeOrRef as SecuritySchemeObject;
      }
    }
  }
  return securitySchemes;
}

/**
 * Returns the list of sanitized keys of non-OAuth2 security schemes
 * required by the operation at the given path and method.
 */
export function getSecurityRequirements(
  operation: OperationObject,
  securitySchemes: { [key: string]: ProcessedSecurityScheme },
  oas: Oas3
): string[] {
  const results: string[] = [];

  // First, consider global requirements
  const globalSecurity: SecurityRequirementObject[] = oas.security;
  if (globalSecurity && typeof globalSecurity !== 'undefined') {
    for (let secReq of globalSecurity) {
      for (let schemaKey in secReq) {
        if (
          securitySchemes[schemaKey] &&
          typeof securitySchemes[schemaKey] === 'object' &&
          securitySchemes[schemaKey].def.type !== 'oauth2'
        ) {
          results.push(schemaKey);
        }
      }
    }
  }

  // Second, consider operation requirements
  const localSecurity: SecurityRequirementObject[] = operation.security;
  if (localSecurity && typeof localSecurity !== 'undefined') {
    for (let secReq of localSecurity) {
      for (let schemaKey in secReq) {
        if (
          securitySchemes[schemaKey] &&
          typeof securitySchemes[schemaKey] === 'object' &&
          securitySchemes[schemaKey].def.type !== 'oauth2'
        ) {
          if (!results.includes(schemaKey)) {
            results.push(schemaKey);
          }
        }
      }
    }
  }
  return results;
}

export enum CaseStyle {
  simple, // No case style is applied. Only illegal characters are removed.
  PascalCase, // Used for type names
  camelCase, // Used for (input) object field names
  ALL_CAPS, // Used for enum values
}

/**
 * Checks to see if the provided string is GraphQL-safe
 */
export function isSanitized(str: string): boolean {
  return /[a-zA-Z0-9_]/gi.test(str);
}

/**
 * First sanitizes given string and then also camelCases it.
 */
export function sanitize(str: string, caseStyle: CaseStyle): string {
  /**
   * Used in conjunction to simpleNames, which only removes illegal
   * characters and preserves casing
   */
  if (caseStyle === CaseStyle.simple) {
    let sanitized = str.replace(/[^a-zA-Z0-9_]/gi, '');

    // Special case: we cannot start with number, and cannot be empty:
    if (/^[0-9]/.test(sanitized) || sanitized === '') {
      sanitized = '_' + sanitized;
    }

    return sanitized;
  }

  /**
   * Remove all GraphQL unsafe characters
   */
  const regex =
    caseStyle === CaseStyle.ALL_CAPS
      ? /[^a-zA-Z0-9_]/g // ALL_CAPS has underscores
      : /[^a-zA-Z0-9]/g;
  let sanitized = str.split(regex).reduce((path, part) => {
    if (caseStyle === CaseStyle.ALL_CAPS) {
      return path + '_' + part;
    } else {
      return path + capitalize(part);
    }
  });

  switch (caseStyle) {
    case CaseStyle.PascalCase:
      // The first character in PascalCase should be uppercase
      sanitized = capitalize(sanitized);
      break;

    case CaseStyle.camelCase:
      // The first character in camelCase should be lowercase
      sanitized = uncapitalize(sanitized);
      break;

    case CaseStyle.ALL_CAPS:
      sanitized = sanitized.toUpperCase();
      break;
  }

  // Special case: we cannot start with number, and cannot be empty:
  if (/^[0-9]/.test(sanitized) || sanitized === '') {
    sanitized = '_' + sanitized;
  }

  return sanitized;
}

/**
 * Sanitizes the given string and stores the sanitized-to-original mapping in
 * the given mapping.
 */
export function storeSaneName(
  saneStr: string,
  str: string,
  mapping: { [key: string]: string }
): string {
  if (saneStr in mapping && str !== mapping[saneStr]) {
    // TODO: Follow warning model
    translationLog(
      `Warning: '${str}' and '${mapping[saneStr]}' both sanitize ` +
        `to '${saneStr}' - collision possible. Desanitize to '${str}'.`
    );
  }
  mapping[saneStr] = str;

  return saneStr;
}

/**
 * Stringifies and possibly trims the given string to the provided length.
 */
export function trim(str: string, length: number): string {
  if (typeof str !== 'string') {
    str = JSON.stringify(str);
  }

  if (str && str.length > length) {
    str = `${str.substring(0, length)}...`;
  }

  return str;
}

/**
 * Determines if the given "method" is indeed an operation. Alternatively, the
 * method could point to other types of information (e.g., parameters, servers).
 */
export function isHttpMethod(method: string): boolean {
  return Object.keys(HTTP_METHODS).includes(method.toLowerCase());
}

/**
 * Formats a string that describes an operation in the form:
 * {name of OAS} {HTTP method in ALL_CAPS} {operation path}
 *
 * Also used in preprocessing.ts where Operation objects are being constructed
 */
export function formatOperationString(
  method: string,
  path: string,
  title?: string
): string {
  if (title) {
    return `${title} ${method.toUpperCase()} ${path}`;
  } else {
    return `${method.toUpperCase()} ${path}`;
  }
}

/**
 * Capitalizes a given string
 */
export function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

/**
 * Uncapitalizes a given string
 */
export function uncapitalize(str: string): string {
  return str.charAt(0).toLowerCase() + str.slice(1);
}

/**
 * For operations that do not have an operationId, generate one
 */
export function generateOperationId(
  method: HTTP_METHODS,
  path: string
): string {
  return sanitize(`${method} ${path}`, CaseStyle.camelCase);
}
