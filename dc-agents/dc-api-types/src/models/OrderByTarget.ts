/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { OrderByColumn } from './OrderByColumn';
import type { OrderBySingleColumnAggregate } from './OrderBySingleColumnAggregate';
import type { OrderByStarCountAggregate } from './OrderByStarCountAggregate';

export type OrderByTarget = (OrderBySingleColumnAggregate | OrderByColumn | OrderByStarCountAggregate);

