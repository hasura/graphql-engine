/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { FunctionInfo } from './FunctionInfo';
import type { ObjectTypeDefinition } from './ObjectTypeDefinition';
import type { TableInfo } from './TableInfo';

export type SchemaResponse = {
  /**
   * Available functions
   */
  functions?: Array<FunctionInfo>;
  /**
   * Object type definitions referenced in this schema
   */
  objectTypes?: Array<ObjectTypeDefinition>;
  /**
   * Available tables
   */
  tables: Array<TableInfo>;
};

