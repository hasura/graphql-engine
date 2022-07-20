/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ColumnCountAggregate } from './ColumnCountAggregate';
import type { SingleColumnAggregate } from './SingleColumnAggregate';
import type { StarCountAggregate } from './StarCountAggregate';

export type Aggregate = (SingleColumnAggregate | ColumnCountAggregate | StarCountAggregate);

