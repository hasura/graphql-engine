/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ScalarType } from './ScalarType';

export type FunctionInformationArgument = {
  /**
   * The name of the argument
   */
  name: string;
  /**
   * If the argument can be omitted
   */
  optional?: boolean;
  type: ScalarType;
};

