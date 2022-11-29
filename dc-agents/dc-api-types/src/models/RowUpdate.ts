/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { IncrementColumnRowUpdate } from './IncrementColumnRowUpdate';
import type { SetColumnRowUpdate } from './SetColumnRowUpdate';

export type RowUpdate = (IncrementColumnRowUpdate | SetColumnRowUpdate);

