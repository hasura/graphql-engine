/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ArrayRelationInsertFieldValue } from './ArrayRelationInsertFieldValue';
import type { ColumnInsertFieldValue } from './ColumnInsertFieldValue';
import type { NullColumnInsertFieldValue } from './NullColumnInsertFieldValue';
import type { ObjectRelationInsertFieldValue } from './ObjectRelationInsertFieldValue';

export type RowObject = Record<string, (ColumnInsertFieldValue | ObjectRelationInsertFieldValue | ArrayRelationInsertFieldValue | NullColumnInsertFieldValue)>;
