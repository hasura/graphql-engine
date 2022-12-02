/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { DeleteMutationOperation } from './DeleteMutationOperation';
import type { InsertMutationOperation } from './InsertMutationOperation';
import type { UpdateMutationOperation } from './UpdateMutationOperation';

export type MutationOperation = (InsertMutationOperation | DeleteMutationOperation | UpdateMutationOperation);

