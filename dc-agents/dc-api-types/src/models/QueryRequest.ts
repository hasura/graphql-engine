/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { InterpolatedQueries } from './InterpolatedQueries';
import type { Query } from './Query';
import type { Relationships } from './Relationships';
import type { ScalarValue } from './ScalarValue';
import type { Target } from './Target';
import type { TargetRedactionExpressions } from './TargetRedactionExpressions';

export type QueryRequest = {
  /**
   * If present, a list of columns and values for the columns that the query must be repeated for, applying the column values as a filter for each query.
   */
  foreach?: Array<Record<string, ScalarValue>> | null;
  interpolated_queries?: InterpolatedQueries;
  query: Query;
  /**
   * Expressions that can be referenced by the query to redact fields/columns
   */
  redaction_expressions?: Array<TargetRedactionExpressions>;
  /**
   * The relationships between tables involved in the entire query request
   */
  relationships: Array<Relationships>;
  target: Target;
};

