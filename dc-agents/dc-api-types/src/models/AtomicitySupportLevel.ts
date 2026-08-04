/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

/**
 * Describes the level of transactional atomicity the agent supports for mutation operations.
 * 'row': If multiple rows are affected in a single operation but one fails, only the failed row's changes will be reverted
 * 'single_operation': If multiple rows are affected in a single operation but one fails, all affected rows in the operation will be reverted
 * 'homogeneous_operations': If multiple operations of only the same type exist in the one mutation request, a failure in one will result in all changes being reverted
 * 'heterogeneous_operations': If multiple operations of any type exist in the one mutation request, a failure in one will result in all changes being reverted
 *
 */
export type AtomicitySupportLevel = 'row' | 'single_operation' | 'homogeneous_operations' | 'heterogeneous_operations';
