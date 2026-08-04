/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

export type ExplainResponse = {
  /**
   * Lines of the formatted explain plan response
   */
  lines: Array<string>;
  /**
   * The generated query - i.e. SQL for a relational DB
   */
  query: string;
};

