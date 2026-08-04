/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Expression } from './Expression';
import type { TargetName } from './TargetName';

export type TargetRedactionExpressions = {
  /**
   * The named redaction expressions associated with the target
   */
  expressions: Record<string, Expression>;
  target: TargetName;
};

