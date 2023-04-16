/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ObjectTypeDefinition } from './ObjectTypeDefinition';
import type { TableInfo } from './TableInfo';

export type SchemaResponse = {
  /**
   * Object type definitions referenced in this schema
   */
  objectTypes?: Array<ObjectTypeDefinition>;
  /**
   * Available tables
   */
  tables: Array<TableInfo>;
};

