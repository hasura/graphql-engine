/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

export type ComparisonColumn = {
  /**
   * The name of the column
   */
  name: string;
  /**
   * The relationship path from the current query table to the table that contains the specified column. Empty array means the current query table.
   */
  path: Array<string>;
};

