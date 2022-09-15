/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

export type ColumnCountAggregate = {
  /**
   * The column to apply the count aggregate function to
   */
  column: string;
  /**
   * Whether or not only distinct items should be counted
   */
  distinct: boolean;
  type: 'column_count';
};

