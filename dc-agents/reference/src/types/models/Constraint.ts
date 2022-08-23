/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

export type Constraint = {
  /**
   * The columns on which you want want to define the foreign key.
   */
    column_mapping: Record<string, string>;
  /**
   * The table referenced by the foreign key in the child table.
   */
  foreign_table: string;
};
