/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ColumnField } from './ColumnField';
import type { NestedObjectField } from './NestedObjectField';
import type { RelationshipField } from './RelationshipField';

export type Field = (NestedObjectField | RelationshipField | ColumnField);

