// Copyright IBM Corp. 2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

import { PreprocessingData } from './types/preprocessing_data';
import { Warning } from './types/options';

export enum MitigationTypes {
  /**
   * Problems with the OAS
   *
   * Should be caught by the module oas-validator
   */
  INVALID_OAS = 'INVALID_OAS',
  UNNAMED_PARAMETER = 'UNNAMED_PARAMETER',

  // General problems
  AMBIGUOUS_UNION_MEMBERS = 'AMBIGUOUS_UNION_MEMBERS',
  CANNOT_GET_FIELD_TYPE = 'CANNOT_GET_FIELD_TYPE',
  COMBINE_SCHEMAS = 'COMBINE_SCHEMAS',
  DUPLICATE_FIELD_NAME = 'DUPLICATE_FIELD_NAME',
  DUPLICATE_LINK_KEY = 'DUPLICATE_LINK_KEY',
  INVALID_HTTP_METHOD = 'INVALID_HTTP_METHOD',
  INPUT_UNION = 'INPUT_UNION',
  MISSING_RESPONSE_SCHEMA = 'MISSING_RESPONSE_SCHEMA',
  MISSING_SCHEMA = 'MISSING_SCHEMA',
  MULTIPLE_RESPONSES = 'MULTIPLE_RESPONSES',
  NON_APPLICATION_JSON_SCHEMA = 'NON_APPLICATION_JSON_SCHEMA',
  OBJECT_MISSING_PROPERTIES = 'OBJECT_MISSING_PROPERTIES',
  UNKNOWN_TARGET_TYPE = 'UNKNOWN_TARGET_TYPE',
  UNRESOLVABLE_SCHEMA = 'UNRESOLVABLE_SCHEMA',
  UNSUPPORTED_HTTP_SECURITY_SCHEME = 'UNSUPPORTED_HTTP_SECURITY_SCHEME',
  UNSUPPORTED_JSON_SCHEMA_KEYWORD = 'UNSUPPORTED_JSON_SCHEMA_KEYWORD',
  CALLBACKS_MULTIPLE_OPERATION_OBJECTS = 'CALLBACKS_MULTIPLE_OPERATION_OBJECTS',

  // Links
  AMBIGUOUS_LINK = 'AMBIGUOUS_LINK',
  LINK_NAME_COLLISION = 'LINK_NAME_COLLISION',
  UNRESOLVABLE_LINK = 'UNRESOLVABLE_LINK',

  // Multiple OAS
  DUPLICATE_OPERATIONID = 'DUPLICATE_OPERATIONID',
  DUPLICATE_SECURITY_SCHEME = 'DUPLICATE_SECURITY_SCHEME',
  MULTIPLE_OAS_SAME_TITLE = 'MULTIPLE_OAS_SAME_TITLE',

  // Options
  CUSTOM_RESOLVER_UNKNOWN_OAS = 'CUSTOM_RESOLVER_UNKNOWN_OAS',
  CUSTOM_RESOLVER_UNKNOWN_PATH_METHOD = 'CUSTOM_RESOLVER_UNKNOWN_PATH_METHOD',
  LIMIT_ARGUMENT_NAME_COLLISION = 'LIMIT_ARGUMENT_NAME_COLLISION',

  // Miscellaneous
  OAUTH_SECURITY_SCHEME = 'OAUTH_SECURITY_SCHEME',
}

export const mitigations: { [mitigationType in MitigationTypes]: string } = {
  /**
   * Problems with the OAS
   *
   * Should be caught by the module oas-validator
   */
  INVALID_OAS: 'Ignore issue and continue.',
  UNNAMED_PARAMETER: 'Ignore parameter.',

  // General problems
  AMBIGUOUS_UNION_MEMBERS: 'Ignore issue and continue.',
  CANNOT_GET_FIELD_TYPE: 'Ignore field and continue.',
  COMBINE_SCHEMAS: 'Ignore combine schema keyword and continue.',
  DUPLICATE_FIELD_NAME: 'Ignore field and maintain preexisting field.',
  DUPLICATE_LINK_KEY: 'Ignore link and maintain preexisting link.',
  INPUT_UNION: 'The data will be stored in an arbitrary JSON type.',
  INVALID_HTTP_METHOD: 'Ignore operation and continue.',
  MISSING_RESPONSE_SCHEMA: 'Ignore operation.',
  MISSING_SCHEMA: 'Use arbitrary JSON type.',
  MULTIPLE_RESPONSES:
    'Select first response object with successful status code (200-299).',
  NON_APPLICATION_JSON_SCHEMA: 'Ignore schema',
  OBJECT_MISSING_PROPERTIES:
    'The (sub-)object will be stored in an arbitrary JSON type.',
  UNKNOWN_TARGET_TYPE: 'The data will be stored in an arbitrary JSON type.',
  UNRESOLVABLE_SCHEMA: 'Ignore and continue. May lead to unexpected behavior.',
  UNSUPPORTED_HTTP_SECURITY_SCHEME: 'Ignore security scheme.',
  UNSUPPORTED_JSON_SCHEMA_KEYWORD: 'Ignore keyword and continue.',
  CALLBACKS_MULTIPLE_OPERATION_OBJECTS: 'Select arbitrary operation object',

  // Links
  AMBIGUOUS_LINK: `Use first occurance of '#/'.`,
  LINK_NAME_COLLISION: 'Ignore link and maintain preexisting field.',
  UNRESOLVABLE_LINK: 'Ignore link.',

  // Multiple OAS
  DUPLICATE_OPERATIONID: 'Ignore operation and maintain preexisting operation.',
  DUPLICATE_SECURITY_SCHEME:
    'Ignore security scheme and maintain preexisting scheme.',
  MULTIPLE_OAS_SAME_TITLE: 'Ignore issue and continue.',

  // Options
  CUSTOM_RESOLVER_UNKNOWN_OAS: 'Ignore this set of custom resolvers.',
  CUSTOM_RESOLVER_UNKNOWN_PATH_METHOD: 'Ignore this set of custom resolvers.',
  LIMIT_ARGUMENT_NAME_COLLISION: `Do not override existing 'limit' argument.`,

  // Miscellaneous
  OAUTH_SECURITY_SCHEME: `Do not create OAuth viewer. OAuth support is provided using the 'tokenJSONpath' option.`,
};

/**
 * Utilities that are specific to OpenAPI-to-GraphQL
 */
export function handleWarning<TSource, TContext, TArgs>({
  mitigationType,
  message,
  mitigationAddendum,
  path,
  data,
  log,
}: {
  mitigationType: MitigationTypes;
  message: string;
  mitigationAddendum?: string;
  path?: string[];
  data: PreprocessingData<TSource, TContext, TArgs>;
  log?: Function;
}) {
  const mitigation = mitigations[mitigationType];

  const warning: Warning = {
    type: mitigationType,
    message,
    mitigation: mitigationAddendum
      ? `${mitigation} ${mitigationAddendum}`
      : mitigation,
  };

  if (path) {
    warning['path'] = path;
  }

  if (data.options.strict) {
    throw new Error(`${warning.type} - ${warning.message}`);
  } else {
    const output = `Warning: ${warning.message} - ${warning.mitigation}`;
    if (typeof log === 'function') {
      log(output);
    } else {
      console.log(output);
    }
    data.options.report.warnings.push(warning);
  }
}

// Code provided by codename- from StackOverflow
// Link: https://stackoverflow.com/a/29622653
export function sortObject<T>(o: T): T {
  return Object.keys(o)
    .sort()
    .reduce((r, k) => ((r[k] = o[k]), r), {}) as T;
}

/**
 * Finds the common property names between two objects
 */
export function getCommonPropertyNames(object1, object2): string[] {
  return Object.keys(object1).filter(propertyName => {
    return propertyName in object2;
  });
}
