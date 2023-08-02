// Copyright IBM Corp. 2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

/**
 * Type definitions for the OpenAPI specification 2.0 (Swagger).
 *
 * NOTE: We do not really care about OpenAPI specification 2.0 / Swagger, as we
 * translate it to Oas3 immediately anyways.
 */

export type Oas2 = {
  swagger: string;
  [key: string]: any;
};
