/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { OpenApiDiscriminator } from './OpenApiDiscriminator';
import type { OpenApiExternalDocumentation } from './OpenApiExternalDocumentation';
import type { OpenApiReference } from './OpenApiReference';
import type { OpenApiXml } from './OpenApiXml';

export type OpenApiSchema = {
  additionalProperties?: any;
  allOf?: Array<(OpenApiSchema | OpenApiReference)>;
  anyOf?: Array<(OpenApiSchema | OpenApiReference)>;
  default?: any;
  deprecated?: boolean;
  description?: string;
  discriminator?: OpenApiDiscriminator;
  enum?: Array<any>;
  example?: any;
  exclusiveMaximum?: boolean;
  exclusiveMinimum?: boolean;
  externalDocs?: OpenApiExternalDocumentation;
  format?: string;
  items?: (OpenApiSchema | OpenApiReference);
  maxItems?: number;
  maxLength?: number;
  maxProperties?: number;
  maximum?: number;
  minItems?: number;
  minLength?: number;
  minProperties?: number;
  minimum?: number;
  multipleOf?: number;
  not?: (OpenApiSchema | OpenApiReference);
  nullable?: boolean;
  oneOf?: Array<(OpenApiSchema | OpenApiReference)>;
  pattern?: string;
  properties?: Record<string, (OpenApiSchema | OpenApiReference)>;
  readOnly?: boolean;
  required?: Array<string>;
  title?: string;
  type?: 'array' | 'boolean' | 'integer' | 'number' | 'object' | 'string';
  uniqueItems?: boolean;
  writeOnly?: boolean;
  xml?: OpenApiXml;
};

