/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ErrorResponseType } from './ErrorResponseType';

export type ErrorResponse = {
  /**
   * Error details
   */
  details?: any;
  /**
   * Error message
   */
  message: string;
  type?: ErrorResponseType;
};

