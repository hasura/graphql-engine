/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { MutationOperation } from './MutationOperation';
import type { Relationships } from './Relationships';
import type { TableInsertSchema } from './TableInsertSchema';
import type { TargetRedactionExpressions } from './TargetRedactionExpressions';

export type MutationRequest = {
  /**
   * The schema by which to interpret row data specified in any insert operations in this request
   */
  insert_schema: Array<TableInsertSchema>;
  /**
   * The mutation operations to perform
   */
  operations: Array<MutationOperation>;
  /**
   * Expressions that can be referenced by the query to redact fields/columns
   */
  redaction_expressions?: Array<TargetRedactionExpressions>;
  /**
   * The relationships involved in the entire mutation request
   */
  relationships: Array<Relationships>;
};

