/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

export type ColumnCountAggregate = {
  /**
   * The columns to apply the count aggregate function to
   */
  columns: Array<string>;
  /**
   * Whether or not only distinct items should be counted
   */
  distinct: boolean;
  type: 'column_count';
};

