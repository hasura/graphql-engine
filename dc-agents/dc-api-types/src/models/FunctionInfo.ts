/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { FunctionInformationArgument } from './FunctionInformationArgument';
import type { FunctionName } from './FunctionName';
import type { FunctionReturnType } from './FunctionReturnType';
import type { FunctionType } from './FunctionType';

export type FunctionInfo = {
  /**
   * argument info - name/types
   */
  args?: Array<FunctionInformationArgument>;
  /**
   * Description of the table
   */
  description?: string | null;
  name: FunctionName;
  /**
   * object response if false, rows if true
   */
  response_cardinality?: 'one' | 'many' | null;
  returns?: FunctionReturnType;
  type: FunctionType;
};

