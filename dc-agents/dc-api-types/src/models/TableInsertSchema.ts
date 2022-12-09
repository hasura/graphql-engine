/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { InsertFieldSchema } from './InsertFieldSchema';
import type { TableName } from './TableName';

export type TableInsertSchema = {
  /**
   * The fields that will be found in the insert row data for the table and the schema for each field
   */
  fields: Record<string, InsertFieldSchema>;
  table: TableName;
};

