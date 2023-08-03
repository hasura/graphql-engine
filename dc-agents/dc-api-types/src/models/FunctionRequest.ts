/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { FunctionName } from './FunctionName';
import type { FunctionRequestArgument } from './FunctionRequestArgument';
import type { Query } from './Query';
import type { Relationships } from './Relationships';
import type { TargetRedactionExpressions } from './TargetRedactionExpressions';

export type FunctionRequest = {
  function: FunctionName;
  /**
   * Function Arguments. TODO. Improve this.
   */
  function_arguments?: Array<FunctionRequestArgument>;
  query: Query;
  /**
   * Expressions that can be referenced by the query to redact fields/columns
   */
  redaction_expressions?: Array<TargetRedactionExpressions>;
  /**
   * The relationships between entities involved in the entire query request
   */
  relationships: Array<Relationships>;
  type: 'function';
};

