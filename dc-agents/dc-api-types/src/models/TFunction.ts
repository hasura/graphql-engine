/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { FunctionName } from './FunctionName';
import type { FunctionRequestArgument } from './FunctionRequestArgument';

export type TFunction = {
  /**
   * The arguments of the function
   */
  arguments: Array<FunctionRequestArgument>;
  name: FunctionName;
  type: 'function';
};

