// Copyright IBM Corp. 2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

/**
 * Type definitions for the OpenAPI Specification 3.
 */

type ExternalDocumentationObject = {
  description?: string;
  url: string;
};

type SchemaObjectType =
  | 'string'
  | 'number'
  | 'integer'
  | 'boolean'
  | 'object'
  | 'array';
export type SchemaObject = {
  title?: string;
  type?: SchemaObjectType | [SchemaObjectType, null];
  format?: string;
  nullable?: boolean;
  description?: string;
  properties?: {
    [key: string]: SchemaObject | ReferenceObject;
  };
  required?: string[];
  default?: any;
  additionalProperties?: SchemaObject | ReferenceObject | boolean;
  items?: SchemaObject | ReferenceObject; // MUST be a single schema object in OAS, see https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#properties
  additionalItems?: boolean | string[];
  enum?: string[];
  allOf?: (SchemaObject | ReferenceObject)[];
  anyOf?: (SchemaObject | ReferenceObject)[];
  oneOf?: (SchemaObject | ReferenceObject)[];
  not?: (SchemaObject | ReferenceObject)[];
};

export type ReferenceObject = {
  $ref: string;
};

type ExampleObject = {
  summary?: string;
  description?: string;
  value?: any;
  externalValue?: string;
};

type HeaderObject = {
  name?: string;
  in?: 'query' | 'header' | 'path' | 'cookie';
  description?: string;
  required?: boolean;
  deprecated?: boolean;
  allowEmptyValue?: boolean;
};

type EncodingObject = {
  contentType?: string;
  headers?: {
    [key: string]: HeaderObject | ReferenceObject;
  };
  style?: string;
  explode?: boolean;
  allowReserved?: boolean;
};

export type MediaTypeObject = {
  schema?: SchemaObject | ReferenceObject;
  example?: any;
  examples?: {
    [key: string]: ExampleObject | ReferenceObject;
  };
  encoding?: {
    [key: string]: EncodingObject;
  };
};

export type ParameterObject = {
  name: string;
  in: 'query' | 'header' | 'path' | 'cookie';
  description?: string;
  required?: boolean;
  deprecated?: boolean;
  allowEmptyValue?: boolean;
  style?: 'form' | 'simple';
  explode?: boolean;
  allowReserved?: boolean;
  schema?: SchemaObject | ReferenceObject;
  example?: any;
  examples?: {
    [key: string]: ExampleObject | ReferenceObject;
  };
  content?: {
    [key: string]: MediaTypeObject;
  };
};

export type MediaTypesObject = {
  [key: string]: MediaTypeObject;
};

export type ServerObject = {
  url: string;
  description?: string;
  variables?: object; // TODO: extend
};

export type RequestBodyObject = {
  description?: string;
  content: {
    [key: string]: MediaTypeObject;
  };
  required?: boolean;
};

export type LinkObject = {
  operationRef?: string;
  operationId?: string;
  parameters?: {
    [key: string]: any;
  };
  requestBody?: any;
  description?: string;
  server?: ServerObject;
};

export type LinksObject = {
  [key: string]: LinkObject | ReferenceObject;
};

export type ResponseObject = {
  description: string;
  headers?: {
    [key: string]: HeaderObject | ReferenceObject;
  };
  content?: MediaTypesObject;
  links?: LinksObject;
};

export type ResponsesObject = {
  [key: string]: ResponseObject | ReferenceObject;
};

export type SecurityRequirementObject = {
  [key: string]: string[];
};

export type OperationObject = {
  tags?: string[];
  summary?: string;
  description?: string;
  externalDocs?: ExternalDocumentationObject;
  operationId?: string;
  parameters?: Array<ParameterObject | ReferenceObject>;
  requestBody?: RequestBodyObject | ReferenceObject;
  responses?: ResponsesObject;
  callbacks?: CallbacksObject;
  deprecated?: boolean;
  security?: SecurityRequirementObject[];
  servers?: ServerObject[];
};

export type PathItemObject = {
  $ref?: string;
  summary?: string;
  description?: string;
  get: OperationObject;
  put: OperationObject;
  post: OperationObject;
  delete: OperationObject;
  options: OperationObject;
  head: OperationObject;
  patch: OperationObject;
  trace: OperationObject;
  servers?: ServerObject[];
  parameters?: [ParameterObject | ReferenceObject];
};

type PathsObject = {
  [key: string]: PathItemObject;
};

export type CallbackObject = {
  [key: string]: PathItemObject;
};

export type CallbacksObject = {
  [key: string]: CallbackObject | ReferenceObject;
};

type OAuthFlowObject = {
  authorizationUrl?: string; // Optional, beacause applies only to certain flows
  tokenUrl?: string; // Optional, beacause applies only to certain flows
  refreshUrl?: string; // Optional, beacause applies only to certain flows
  scopes?: {
    // Optional, beacause applies only to certain flows
    [key: string]: string;
  };
};

type OAuthFlowsObject = {
  implicit?: OAuthFlowObject;
  password?: OAuthFlowObject;
  clientCredentials?: OAuthFlowObject;
  authorizationCode?: OAuthFlowObject;
};

export type SecuritySchemeObject = {
  type: 'apiKey' | 'http' | 'oauth2' | 'openIdConnect';
  description?: string;
  name?: string; // Optional, because applies only to apiKey
  in?: string; // Optional, because applies only to apiKey
  scheme?: string; // Optional, because applies only to http
  bearerFormat?: string;
  flows?: OAuthFlowsObject; // Optional, because applies only to oauth2
  openIdConnectUrl?: string; // // Optional, because applies only to openIdConnect
};

export type SecuritySchemesObject = {
  [key: string]: SecuritySchemeObject | ReferenceObject;
};

type ComponentsObject = {
  schemas?: {
    [key: string]: SchemaObject | ReferenceObject;
  };
  responses?: ResponsesObject;
  parameters?: {
    [key: string]: ParameterObject | ReferenceObject;
  };
  examples?: {
    [key: string]: ExampleObject | ReferenceObject;
  };
  requestBodies?: {
    [key: string]: RequestBodyObject | ReferenceObject;
  };
  headers?: {
    [key: string]: HeaderObject | ReferenceObject;
  };
  securitySchemes?: SecuritySchemesObject;
  links?: LinksObject;
  callbacks?: {
    [key: string]: CallbackObject | ReferenceObject;
  };
};

type TagObject = {
  name: string;
  description?: string;
  externalDocs?: ExternalDocumentationObject;
};

export type Oas3 = {
  openapi: string;
  info: {
    title: string;
    description?: string;
    termsOfService?: string;
    contact?: {
      name?: string;
      url?: string;
      email?: string;
    };
    license?: {
      name: string;
      url?: string;
    };
    version: string;
  };
  servers?: ServerObject[];
  paths: PathsObject;
  components?: ComponentsObject;
  security?: SecurityRequirementObject[];
  tags?: TagObject[];
  externalDocs?: ExternalDocumentationObject;
};
