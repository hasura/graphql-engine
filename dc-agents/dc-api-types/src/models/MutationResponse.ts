/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { MutationOperationResults } from './MutationOperationResults';

export type MutationResponse = {
  /**
   * The results of each mutation operation, in the same order as they were received
   */
  operation_results: Array<MutationOperationResults>;
};

