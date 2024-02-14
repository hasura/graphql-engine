/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ArgumentValue } from './ArgumentValue';

export type NamedArgument = {
  /**
   * The name of the named argument
   */
  name: string;
  type: 'named';
  value: ArgumentValue;
};

