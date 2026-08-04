/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ColumnTypeNonScalar } from './ColumnTypeNonScalar';
import type { ScalarType } from './ScalarType';

export type ColumnType = (ScalarType | ColumnTypeNonScalar);

