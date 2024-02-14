/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { FunctionRelationships } from './FunctionRelationships';
import type { InterpolatedRelationships } from './InterpolatedRelationships';
import type { TableRelationships } from './TableRelationships';

export type Relationships = (InterpolatedRelationships | FunctionRelationships | TableRelationships);

