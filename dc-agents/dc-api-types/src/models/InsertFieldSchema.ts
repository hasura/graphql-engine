/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ArrayRelationInsertSchema } from './ArrayRelationInsertSchema';
import type { ColumnInsertSchema } from './ColumnInsertSchema';
import type { ObjectRelationInsertSchema } from './ObjectRelationInsertSchema';

export type InsertFieldSchema = (ObjectRelationInsertSchema | ArrayRelationInsertSchema | ColumnInsertSchema);

