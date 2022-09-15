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
   * Column name
   */
  name: string;
  /**
   * Is column nullable
   */
  nullable: boolean;
  type: ScalarType;
};

