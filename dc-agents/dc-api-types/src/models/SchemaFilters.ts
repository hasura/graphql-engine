/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { FunctionName } from './FunctionName';
import type { TableName } from './TableName';

export type SchemaFilters = {
  /**
   * Only get the schemas for these functions
   */
  only_functions?: Array<FunctionName>;
  /**
   * Only get the schemas for these tables
   */
  only_tables?: Array<TableName>;
};

