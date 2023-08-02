/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { FunctionName } from './FunctionName';
import type { FunctionRequestArgument } from './FunctionRequestArgument';
import type { Query } from './Query';
import type { Relationships } from './Relationships';

export type FunctionRequest = {
  function: FunctionName;
  /**
   * Function Arguments. TODO. Improve this.
   */
  function_arguments?: Array<FunctionRequestArgument>;
  query: Query;
  /**
   * The relationships between entities involved in the entire query request
   */
  relationships: Array<Relationships>;
  type: 'function';
};

