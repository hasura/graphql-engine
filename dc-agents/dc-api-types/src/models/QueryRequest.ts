/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { FunctionRequest } from './FunctionRequest';
import type { TableRequest } from './TableRequest';

export type QueryRequest = (FunctionRequest | TableRequest);

