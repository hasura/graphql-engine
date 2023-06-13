// Copyright IBM Corp. 2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

/**
 * Type definitions for the data created during preprocessing.
 */

import { Operation, DataDefinition } from './operation';
import { InternalOptions } from './options';
import { SecuritySchemeObject, SchemaObject, Oas3, LinkObject } from './oas3';

export type ProcessedSecurityScheme = {
  rawName: string;
  def: SecuritySchemeObject;

  /**
   * Stores the names of the authentication credentials
   * NOTE: Structure depends on the type of the protocol (basic, API key...)
   * NOTE: Mainly used for the AnyAuth viewers
   * NOTE: Values are sanitized (see getProcessedSecuritySchemes() in preprocessor.ts)
   */
  parameters: { [key: string]: string };

  /**
   * JSON schema to create the viewer for this security scheme from.
   */
  schema: SchemaObject;

  /**
   * The OAS which this operation originated from
   */
  oas: Oas3;
};

export type PreprocessingData<TSource, TContext, TArgs> = {
  /**
   * List of operation objects
   */
  operations: { [key: string]: Operation };

  /**
   * List of Operation objects
   */
  callbackOperations: { [key: string]: Operation };

  /**
   * List of all the used object names to avoid collision
   */
  usedTypeNames: string[];

  /**
   * List of data definitions for JSON schemas already used.
   */
  defs: DataDefinition[];

  /**
   * The security definitions contained in the OAS. References are resolved.
   *
   * NOTE: Keys are sanitized
   * NOTE: Does not contain OAuth 2.0-related security schemes
   */
  security: { [key: string]: ProcessedSecurityScheme };

  /**
   * Mapping between sanitized strings and their original ones
   */
  saneMap: { [key: string]: string };

  /**
   * Options passed to OpenAPI-to-GraphQL by the user
   */
  options: InternalOptions<TSource, TContext, TArgs>;

  /**
   * All of the provided OASs
   */
  oass: Oas3[];
};
