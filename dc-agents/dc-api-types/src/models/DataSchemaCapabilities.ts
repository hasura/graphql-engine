/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ColumnNullability } from './ColumnNullability';

export type DataSchemaCapabilities = {
  column_nullability?: ColumnNullability;
  /**
   * Whether tables can have foreign keys
   */
  supports_foreign_keys?: boolean;
  /**
   * Whether tables can have primary keys
   */
  supports_primary_keys?: boolean;
  /**
   * Whether the database supports tables with no defined schema
   */
  supports_schemaless_tables?: boolean;
};

