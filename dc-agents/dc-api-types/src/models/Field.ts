/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ColumnField } from './ColumnField';
import type { NestedArrayField } from './NestedArrayField';
import type { NestedObjField } from './NestedObjField';
import type { RelationshipField } from './RelationshipField';

export type Field = (NestedArrayField | NestedObjField | ColumnField | RelationshipField);

