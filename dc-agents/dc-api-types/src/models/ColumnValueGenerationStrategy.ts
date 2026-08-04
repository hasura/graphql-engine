/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { AutoIncrementGenerationStrategy } from './AutoIncrementGenerationStrategy';
import type { DefaultValueGenerationStrategy } from './DefaultValueGenerationStrategy';
import type { UniqueIdentifierGenerationStrategy } from './UniqueIdentifierGenerationStrategy';

export type ColumnValueGenerationStrategy = (DefaultValueGenerationStrategy | AutoIncrementGenerationStrategy | UniqueIdentifierGenerationStrategy);

