/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExistsInTable } from './ExistsInTable';
import type { Expression } from './Expression';

export type ExistsExpression = {
  in_table: ExistsInTable;
  type: 'exists';
  where: Expression;
};

