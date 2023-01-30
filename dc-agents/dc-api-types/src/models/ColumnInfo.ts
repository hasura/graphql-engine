/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ScalarType } from './ScalarType';

export type ColumnInfo = {
  /**
   * Column description
   */
  description?: string | null;
  /**
   * Whether or not the column can be inserted into
   */
  insertable?: boolean;
  /**
   * Column name
   */
  name: string;
  /**
   * Is column nullable
   */
  nullable: boolean;
  type: ScalarType;
  /**
   * Whether or not the column can be updated
   */
  updatable?: boolean;
};

